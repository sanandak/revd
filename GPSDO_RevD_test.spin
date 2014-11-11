'GPSDO Test Harness


CON
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  CLK_FREQ = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS   = CLK_FREQ / 1_000                                   ' ticks in 1ms
  POINT_SEVEN_SECONDS = 700 * ONE_MS

  ORSOC_EN      = 4
  GPSDO_REF     = 20_000_000  
  EXPANDER_1    = %0111_111_0   ' bits 7-4 are taken from Datasheet; bits 4-1 are set by layout
  EXPANDER_2    = %0111_110_0   ' bits 7-4 are taken from Datasheet; bits 4-1 are set by layout
  'I2C_2         = %0111_110_0   ' bits 7-4 are taken from Datasheet; bits 4-1 are set by layout

  BIT_0    = %0000_0001
  BIT_1    = %0000_0010
  BIT_2    = %0000_0100
  BIT_3    = %0000_1000
  BIT_4    = %0001_0000
  BIT_5    = %0010_0000
  BIT_6    = %0100_0000
  BIT_7    = %1000_0000

  MUX1SEL_1_2 = BIT_0
  MUX1SEL_3_4 = BIT_1
  MUX2SEL_1_2 = BIT_2
  MUX2SEL_3_4 = BIT_3
  PGA_1_CS    = BIT_4
  PGA_2_CS    = BIT_5
  PGA_3_CS    = BIT_6
  PGA_4_CS    = BIT_7

  LED_1       = BIT_0
  LED_2       = BIT_1
  EN_5V       = BIT_2
  WIFI_EN     = BIT_3
  UBLOX_EN    = BIT_4
  OLED_EN     = BIT_5
  MAG_EN      = BIT_6
  GPS_RESET   = BIT_7
  
  SCL      = 28             'I2C clock pin#
  SDA      = 29             'I2C data pin#



CON ' GPS constants including bit on I2C expander
  DEBUG             = 0
  GPS_PORT          = 1
  DEBUG_BAUD        = 115200
  GPS_BAUD          = 9600

  EXTERNAL_OSC      = 0
  PERIOD_LENGTH     = 16  ' number of seconds over which to collect oscillator data
  GPS_PPS           = 1
  GPS_RX_FROM       = 2
  GPS_TX_TO         = 3

CON
  ' DISPLAY CONSTANTS
  CLEAR_DISPLAY  = %00_0000_0001
  RETURN_HOME    = %00_0000_0010
  ENTRY_MODE_0   = %00_0000_0100  ' auto-DEincrement no shift
  ENTRY_MODE_1   = %00_0000_0101  ' auto-DEcrement   shift display RIGHT
  ENTRY_MODE_2   = %00_0000_0110  ' auto-INcrement   no shift
  ENTRY_MODE_3   = %00_0000_0111  ' auto-INcrememnt  shift display LEFT
  DISPLAY_ON     = %00_0000_1101  ' display ON   cursor OFF  blinking OFF
  DISPLAY_OFF    = %00_0000_1000  ' display OFF
  FUNCTION_SET   = %00_0011_1000  ' set to english
  CURSOR_SHIFT_0 = %00_0001_0000  ' shift CURSOR to the left
  CURSOR_SHIFT_1 = %00_0001_0100  ' shift CURSOR to the right
  CURSOR_SHIFT_2 = %00_0001_1000  ' shift DISPLAY to the left
  CURSOR_SHIFT_3 = %00_0001_1100  ' shift DISPLAY to the right
  SET_CGRAM_ADD  = %00_0100_0000  ' lower 6 bits of this command should be the address
  SET_DDRAM_ADD  = %00_1000_0000  ' lower 7 bits of this command should be the address
  DEGREES        = %1101_1111
  TICK           = %0010_0111
  BLOCK          = %1111_1111     ' all pixel high

CON
  LOW               = 0
  HIGH              = 1
  INPUT             = 0
  OUTPUT            = 1

  CR                = 13
  TAB               =  9
  SPACE             = 32

  DEBUG_RX_FROM     = 31
  DEBUG_TX_TO       = 30
CON ' Temp sensor constants
  RD_ROM     = $33                                              ' 1W ROM commands
  MATCH_ROM  = $55
  SKIP_ROM   = $CC
  SRCH_ROM   = $F0
  ALARM_SRCH = $EC
  
  CVRT_TEMP  = $44                                              ' DS1822 commands
  WR_SPAD    = $4E
  RD_SPAD    = $BE
  COPY_SPAD  = $48 
  RD_EE      = $B8
  RD_POWER   = $B4
  
  OW_PIN     = 27  ' pin for one-wire bus
  
VAR
  byte expVal1, expVal2
  byte sharedSclk, sharedMosi, sharedMiso
  byte  snum[8]

  word dacValue, dacNew, dacOld

  long change, error, k
  
OBJ  'Objects Section
  uarts    : "FullDuplexSerial4portPlus_0v3"       '1 COG for 3 serial ports
  PEBBLE   : "BasicPebbleFunctions"                ' I put these into a seperate file to make editing easier
  UBX      : "ubloxInterface2"
  Num      : "Numbers"                             ' Include Numbers object for writing numbers to debuginal
  ow       : "jm_1-wire"

DAT
  oledClrLine byte  "                ", 0
  oledLine1  byte   "                ", 0
  oledLine2  byte   "                ", 0

PUB MAIN | i, risingEdge, fallingEdge, timeout, rxByte, n, p, status, temp, crc, t1, t2
  DIRA~ ' set everything to input

  DIRA[20] := 1
  DIRA[19] := 1

  PEBBLE.INIT
  PEBBLE.GUMSTIX_OFF

  PEBBLE.LED1_ON

  LAUNCH_SERIAL_COG             ' handle the 2 serial ports- debug and GPS

'  uarts.str(DEBUG,string(13, "  ---- geoPebble GPSDO Demo      ----"))
'  uarts.str(DEBUG,string(13, "  ---- Setting up Shared SPI bus ----"))  
'  SETUP_SHARED_SPI
'  PAUSE_MS(1000)
  

  PEBBLE.ANALOG_ON
  'PEBBLE.GUMSTIX_ON
  PEBBLE.GUMSTIX_OFF
  PEBBLE.GPS_ON
  
  'PASS_THROUGH(GPS_PORT)
  UBX.INIT(DEBUG, GPS_PORT)          

  PAUSE_MS(100)
  UBX.VERSION_POLL
  UBX.TURN_OFF_NMEA
  UBX.TURN_ON_RAW
  UBX.TURN_ON_PPS


  uarts.str(DEBUG,string(13, "  ---- Setting up Temp Sensor  ----"))
  uarts.putc(DEBUG, CR) 
  OW.INIT(OW_PIN)                                                    ' DS1820 on P0
  status := OW.RESET                                            ' check for device


  CTRA := %01010 << 26 + EXTERNAL_OSC            ' counter A in POSedge mode
  FRQA := 1                                     ' increment once per pulse
  CTRB := %01110 << 26 + EXTERNAL_OSC
  FRQB := 1
' *************************
' * Display deviceList
' *************************
  uarts.str(DEBUG, string(13,"OneWire devices found: ",13))
  if (status == %10)                                            ' good 1W reset
    READ_SN(@snum)                                               ' read the serial #
    SHOW_SN(@snum)                                               ' show it
    crc := ow.crc8(@snum, 7)                                    ' calculate CRC
    if (crc <> snum[7])                                         ' compare with CRC in SN
      UARTS.STR(DEBUG, string("    "))
      UARTS.HEX(DEBUG, crc, 2)
      UARTS.STR(DEBUG, string(" - bad CRC"))
      repeat
  else
    case status
      %00 : UARTS.STR(DEBUG, string("-- Buss short"))
      %01 : UARTS.STR(DEBUG, string("-- Buss interference"))
      %11 : UARTS.STR(DEBUG, string("-- No device"))


  PAUSE_MS(200)

  uarts.str(DEBUG,string(13,"Waiting for PPS before disclipining VCXO."))
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high

  uarts.str(DEBUG,string(13, "  ---- Calibrating GPSDO  ----"))  

  CALIBRATE_GPSDO
'  repeat
'    waitcnt(0)

  dacValue := $7FFF       ' set to middle of the road value
  dacOld := dacValue
  uarts.str(DEBUG,string(13,"DAC start value: "))
  uarts.dec(DEBUG,dacValue)
  PEBBLE.WRITE_TO_DAC(dacValue)     ' put a middle of the road value in here to start with

  CALIBRATE_SYSTEM_CLOCK
   
  ' DAC values:20Mhz values range from 0:19_999_615 to 65024:20_000_244
  uarts.str(DEBUG,string(13,"Waiting for PPS before disclipining VCXO."))
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0
  PHSB := 0
  i := 0
  repeat
    t1 := 0
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    't1 -= cnt
    fallingEdge := PHSB
    risingEdge  := PHSA    ' this capture of PHSA takes 1136 clock counts
    t1 += cnt
    if risingEdge < (GPSDO_REF+GPSDO_REF>>1)  ' could we have one or more PPS edges?
      error := (risingEdge - GPSDO_REF)
      dacNew := dacOld - K * error
      temp := READ_TC                                          ' read the temperature
    
      uarts.putc(DEBUG, 13)
      uarts.dec(DEBUG,i++)
      uarts.putc(DEBUG, TAB)
      SHOW_C(temp)                                             ' display in °C
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,risingEdge)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,fallingEdge)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,error)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,dacOld)
      uarts.putc(DEBUG, TAB)
      uarts.dec(DEBUG,dacNew)
      uarts.putc(DEBUG, TAB)
      PEBBLE.WRITE_TO_DAC(dacNew)
'      PEBBLE.WRITE_TO_OLED(0, 1, oledClrLine)
'      PEBBLE.WRITE_TO_OLED(0, 1, string("What's up?"))
      

      
      dacOld := dacNew
     
    
    t2 := 0
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    't2 -= cnt
    PHSB := 0    ' clearing PHSA takes 1136 clock counts
    PHSA := 0    ' clearing PHSA takes 1136 clock counts
    t2 += cnt
    

PUB CALIBRATE_GPSDO | highDac, lowDac, highCount, lowCount
' get a couple readings from the GPSDO to set a linear frequncy/DAC relationship
  uarts.putc(DEBUG, 13)
  highDAC := $BFFF
'  highDAC := $FFFF
  PEBBLE.WRITE_TO_DAC(highDAC)  ' set DAC to 3/4 full-scale value
  PAUSE_MS(2000)       ' let it settle
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  repeat 20
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    highCount := PHSA  ' an adjustment based on comparing prop counts to freq counter
    PHSA := 0
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,highDAC)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,highCount)
    uarts.putc(DEBUG, 13)

  lowDAC := $3FFF
'  lowDAC := 0
  uarts.putc(DEBUG, 13)
  PEBBLE.WRITE_TO_DAC(lowDAC)  ' set DAC to 1/4 full-scale value
  PAUSE_MS(2000)       ' let it settle
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  repeat 20
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    lowCount := PHSA  ' an adjustment based on comparing prop counts to freq counter
    PHSA := 0

    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,lowDAC)
    uarts.putc(DEBUG, TAB)
    uarts.dec(DEBUG,lowCount)
    uarts.putc(DEBUG, 13)

  k := (highDAC-lowDAC)/(highCount-lowCount)
  uarts.str(DEBUG,string(13,"K="))
  uarts.dec(DEBUG, k)
  uarts.putc(DEBUG, 13)

PUB DISCPLINED_OSCILLATOR | risingEdge
' sit here endlessly checking the number of 20Mhz pulses between consecutive PPS
  CTRA := %01010 << 26 + EXTERNAL_OSC            ' counter A in POSedge mode
  FRQA := 1                                     ' increment once per pulse
  
  waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
  waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
  PHSA := 0

  repeat
    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    risingEdge := PHSA
    uarts.str(DEBUG,string(13,"Edges this period: "))
    uarts.dec(DEBUG,risingEdge)

    waitpeq(0, constant(|<GPS_PPS), 0)    ' wait for pin to go low
    waitpne(0, constant(|<GPS_PPS), 0)    ' wait for pin to go high
    PHSA := 0


PUB PASS_THROUGH (port) | Rx_byte
' infinite loop which simply reads from port and spits it to port 0 which should be the PST.

  repeat
    Rx_byte := uarts.rxcheck(port)       ' collect byte from MACM port
    if Rx_byte <> -1                     ' -1 means no data; don't do anything
      uarts.putc(debug,Rx_byte)

    Rx_byte := uarts.rxcheck(debug)       ' collect byte from MACM port
    if Rx_byte <> -1                     ' -1 means no data; don't do anything
      uarts.putc(port,Rx_byte)

          
PUB LAUNCH_SERIAL_COG
'' method that sets up the serial ports
  NUM.INIT
  UARTS.INIT
  UARTS.ADDPORT(DEBUG,    DEBUG_RX_FROM, DEBUG_TX_TO, -1, -1, 0, %000000, DEBUG_BAUD)    'Add DEBUG port
  UARTS.ADDPORT(GPS_PORT, GPS_RX_FROM,   GPS_TX_TO,   -1, -1, 0, %000000, GPS_BAUD)      'Add condor NMEA port
  UARTS.START

{PUB SETUP_SHARED_SPI
' method that sets up the shared SPI bus
' deselects ALL chip selects so things are ready but not selected

{{
DESCRIPTION: This method initializes the SPI IO's, places the all devices on the shared
             bus into the DE-SELECTED state
RETURNS:     TRUE if sucessful; otherwise FALSE.
}}
      
  sharedSclk   := SHARED_SCLK
  sharedMosi   := SHARED_MOSI
  sharedMiso   := SHARED_MISO

' SETUP SPI PINS
  SETUP_PIN(sharedSclk,  OUTPUT, LOW)
  SETUP_PIN(sharedMosi,  OUTPUT, LOW)
  SETUP_PIN(sharedMiso,  INPUT,  LOW)
  SETUP_PIN(DAC_CS,      OUTPUT, HIGH)
  SETUP_PIN(OLED_CS,     OUTPUT, HIGH)

PUB WRITE_TO_DAC(newDacValue)| dacCode
' method that clocks out 24 bits to the DAC.  This command is a "Write to and Update" variety
  dacCode := WRITE_AND_UPDATE | (newDacValue & $FFFF)
  'uarts.str(DEBUG,string( " sending to DAC: "))
  uarts.putc(DEBUG, SPACE)
  uarts.hex(DEBUG,dacCode, 6)
  dacCode ><= 24          ' reverse the 24 bits of interest with MSB now in bit 0

  OUTA[DAC_CS]  := 0                                    ' lower CS line
  repeat 24
    OUTA[sharedSclk] := 0                                    ' drop clock
    OUTA[sharedMosi] := (dacCode & %01)                      ' place next bit on MOSI
    dacCode >>= 1                                            ' shift right by one
    OUTA[sharedSclk] := 1                                    ' raise clock
         
  OUTA[sharedMosi] := 0
  OUTA[sharedSclk] := 0

  OUTA[DAC_CS]  := 1                                    ' raise CS line
  
}  
PUB CALIBRATE_SYSTEM_CLOCK | e_time, b_time, diff
  repeat 15
    waitpne(0,constant(|<GPS_PPS), 0)    ' wait for pin to go high
    e_time := cnt

    diff := e_time - b_time
    uarts.str(DEBUG,string(13,"  Number of cnts:   "))
    uarts.dec(DEBUG, diff)

    b_time := e_time
    PAUSE_MS(200)
{    
PUB GOTO_XY(xPos, yPos) | position
'' method to position cursor
'' treats top line (upper most) is line ypos=1, lower line is ypos=2
'' far left of display is xpos=0; far right is xpos=15
  position := xPos <# $0F
  if yPos == 2
    position += $40
  position |= $80
  WRITE_OLED_COMMAND(OLED_CS, position)


PUB WRITE_STRING(stringPtr)
  OLED_INIT
  repeat strsize(stringPtr)
    WRITE_DATA(OLED_CS, byte[stringPtr++])

  OUTA[OLED_CS]  := 1                                    ' raise CS line

PUB OLED_INIT 
{{

}}
  ' lower CS line
  OUTA[OLED_CS]  := 0                                    ' lower CS line

  ' RS bit; there is no valid data on input during RS bit
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := 1                                    ' place 1 on MOSI
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' RW bit; there is no valid data on input during RW bit
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := 0                                    ' place 0 on MOSI
  OUTA[sharedSclk] := 1                                    ' raise clock

PUB WRITE_OLED_COMMAND(csLine, data_out) | data_in
{{
DESCRIPTION: This method reads data from the SPI lines while writing (a bit at a time)
(SPI is a circular buffer protocal), This is an optimized 10-bit read/write
specific to this LCD

PARMS:

data_out : source of data to transmit

RETURNS: data retrieved from SPI transmission

}}
  ' clear result
  data_in := 0

  ' optimize code for 10-bit case by unrolling loop, if other bit lengths occur frequently unroll as well

  ' lower CS line
  OUTA[csLine]     := 0                                    ' lower CS line

  ' RS bit; there is no valid data on input during RS bit during first instruction
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := 0                                    ' place zero bit on MOSI
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' RW bit; there is no valid data on input during RW bit during first instruction
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := 0                                    ' place zero bit on MOSI
  OUTA[sharedSclk] := 1                                    ' raise clock


  ' now get into the data portion of the Instructions (the lower 8 bits)
  ' bit 7
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 7)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 6
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 6)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 5
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 5)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 4
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 4)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 3
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 3)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 2
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 2)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 1
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 1)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' bit 0
  OUTA[sharedSclk] := 0                                    ' drop clock
  OUTA[sharedMosi] := data_out                             ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk] := 1                                    ' raise clock
     
  ' set clock and MOSI to LOW on exit
  OUTA[sharedMosi] := 0
  OUTA[sharedSclk] := 0

  ' raise CS on exit
  OUTA[csLine]     := 1                                    ' raise CS line
  
  ' at this point, the data has been written and read, return result
  return ( data_in )            

PUB WRITE_DATA(csLine, data_out) | data_in
{{
DESCRIPTION: This method reads data from the SPI lines while writing (a bit at a time)
(SPI is a circular buffer protocal), This is an optimized 8-bit read/write
specific to this LCD

PARMS:

data_out : source of data to transmit

RETURNS: data retrieved from SPI transmission

}}
  ' clear result
  data_in := 0

  ' now get into the data portion of the Instructions (the lower 8 bits)
  ' bit 7
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 7)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 6
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 6)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 5
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 5)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 4
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 4)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 3
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 3)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 2
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 2)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 1
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := (data_out >> 1)                      ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' bit 0
  OUTA[sharedSclk]  := 0                                    ' drop clock
  OUTA[sharedMosi] := data_out                             ' place next bit on MOSI
  data_in := (data_in << 1) + INA[sharedMiso]              ' now read next bit from MISO
  OUTA[sharedSclk]  := 1                                    ' raise clock
     
  ' set clock and MOSI to LOW on exit
  OUTA[sharedMosi]  := 0
  OUTA[sharedSclk]   := 0

  ' at this point, the data has been written and read, return result
  return ( data_in )            

PUB SETUP_PIN(pin, dir, state)
' use the dir and state to set the state and direction of PIN
  if state == LOW
    OUTA[pin] := 0 '  low

  if state == HIGH
    OUTA[pin] := 1 '  high

  if dir == OUTPUT
    DIRA[pin] := 1 '  output

  if dir == INPUT
    DIRA[pin] := 0 '  input
}
PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

PUB READ_SN(pntr)

'' Reads serial number from 1W device
'' -- stores in array at pntr

  OW.RESET
  OW.WRITE(RD_ROM)
  repeat 8
    byte[pntr++] := OW.READ

  
PUB SHOW_SN(pntr)

'' Displays 1W serial number stored in array at pntr

  UARTS.STR(DEBUG, string("SN: "))
  pntr += 7                                                     ' move to MSB
  repeat 8
    UARTS.HEX(DEBUG, byte[pntr--], 2)                                   ' print MSB to LSB, l-to-r
  UARTS.PUTC(DEBUG, CR) 


PUB READ_TC | tc

'' Reads temperature from DS1820
'' -- returns degrees C in 0.1 degree units

  OW.RESET               
  OW.WRITE(SKIP_ROM)          
  OW.WRITE(CVRT_TEMP)          
  repeat                                                        ' let conversion finish                 
    tc := OW.RDBIT     
  until (tc == 1)    
  OW.RESET               
  OW.WRITE(SKIP_ROM)          
  OW.WRITE(RD_SPAD)          
  tc := OW.READ                                                 ' lsb of temp      
  tc |= OW.READ << 8                                            ' msb of temp
  OW.RESET

  tc := ~~tc * 5                                                ' extend sign, 0.1° units

  return tc


PUB SHOW_C(temp)

'' Print temperature in 0.1 degrees Celsius 

  if (temp < 0)                                                 ' if negative
    UARTS.PUTC(DEBUG, "-")                                                ' print sign
    ||temp                                                      ' and make positive
    
  UARTS.DEC(DEBUG, temp / 100)                                           ' print whole portion
  UARTS.PUTC(DEBUG, ".")
  UARTS.DEC(DEBUG, temp // 100)                                          ' print fractional portion
  'UARTS.PUTC(DEBUG, 176)
  'UARTS.PUTC(DEBUG,"C")
'  term.tx(CLREOL)
'  term.tx(CR)


PUB SHOW_F(temp) | tp

'' Print temperature in 0.1 degrees Fahrenheit
'' -- temp is passed in C

  if (temp > 0)
    temp := temp * 9 / 5 + 32_0
  else
    temp := 32_0 - (||temp * 9 / 5)
    
  if (temp < 0)
    UARTS.PUTC(DEBUG,"-")
    ||temp
    
  UARTS.DEC(DEBUG, temp / 10)
  UARTS.PUTC(DEBUG,".")
  UARTS.DEC(DEBUG,temp // 10)
  'UARTS.PUTC(DEBUG,176)
  'UARTS.PUTC(DEBUG,"F")
  'term.tx(CLREOL)
  'term.tx(CR)  

DAT
ds2401_name     byte    "DS2401  ", 0
ds2405_name     byte    "DS2405  ", 0
ds2450_name     byte    "DS2450  ", 0
ds1820_name     byte    "DS1820  ", 0
ds18b20_name    byte    "DS18B20 ", 0
ds1822_name     byte    "DS1822  ", 0
unknown_name    byte    "  ??    ", 0