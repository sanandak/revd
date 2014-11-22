'**************************************************************
'* RevD_pebble_code.spin
'* Main and top level program for operating the RevC board.
'*   built using the different test codes developed for RevC
'*
'* PGB
'* 3 October 2014 - based on RevC_pebble_code
'*
'*
'**************************************************************
CON ' Clock mode settings
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  CLK_FREQ = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS   = CLK_FREQ / 1_000                                   ' ticks in 1ms
  ONE_US   = CLK_FREQ / 1_000_000                               ' ticks in 1us
 
                                 
CON ' VERSION
  VERSION = $01 ' this is a single byte    

CON ' GPS constants including bit on I2C expander
  EXTERNAL_OSC      = 0
  PERIOD_LENGTH     = 16  ' number of seconds over which to collect oscillator data
  GPS_PPS           = 1
  GPS_RX_FROM       = 2
  GPS_TX_TO         = 3

  BYTES_IN_GPS_BUFFER = 1024

CON ' Uart command constants
    KILL                  = "K"
    SHUTDOWN_SYSTEM       = "D"
    SOFT_REBOOT_GUMSTIX   = "S"
    HARD_REBOOT_GUMSTIX   = "H"
    PUT_SYSTEM_TO_SLEEP   = "L"
    RATE                  = "R"
    QUERY                 = "Q"
    GAIN                  = "G"
    SOURCE                = "O"
    WRITE_TO_OLED         = "W"

  #0, WAITING_FOR_START, WAITING_FOR_END, PROCESS_BUFFER
  
CON ' ON/OFF switch constants
  REED_SWITCH       = 15
  BUTTON_PRESS_TIME = CLK_FREQ
  BUTTON_HOLD_TIME  = CLK_FREQ*5
  NOT_PRESSED       = 1         ' Reed switch is pulled high normally 
  PRESSED           = 0         ' Pressed is actually logic 0  

CON ' ADC constants
  
  ADC_DRDYOUT =  5
  ADC_MISO    =  6 ' connected to DOUT on MAX11040K
  ADC_MOSI    =  7 ' connected to DIN  on MAX11040K
  ADC_SCLK    =  8
  ADC_CS      =  9

  
  WSCIR   = %0100_0000  ' Write Sampling Instant Control Register
  RSICR   = %1100_0000  ' Read Sampling Instant Control Register
  WDRCR   = %0101_0000  ' Write Data-Rate Control Register
  RDRCR   = %1101_0000  ' Read Data-Rate Control Register
  WCR     = %0110_0000  ' Write Configuration Register
  R_CR    = %1110_0000  ' Read Configuration Register
  RDR     = %1111_0000  ' Read Data Register

CON ' PGA constants
  GAIN_A = 1
  GAIN_B = 10
  GAIN_C = 10
  GAIN_D = 10

CON ' MUX constants

  INTERNAL = 0
  EXTERNAL = 1  

CON ' channel constants
  CHANNEL_A = %0001
  CHANNEL_B = %0010
  CHANNEL_C = %0100
  CHANNEL_D = %1000
  
CON ' constants for spi buffers
  BYTES_IN_SLAVE_BUFFER   = 512
  LONGS_IN_SLAVE_BUFFER   = BYTES_IN_SLAVE_BUFFER >> 2


  SLAVE_CLK           = 16 
  SLAVE_SIMO          = 17 
  SLAVE_SOMI          = 18 
  SLAVE_CS            = 19 
  SLAVE_IRQ           = 20 
  

  SLAVE_TEST          = 4 ' use spare pin as test point
'  MASTER_TEST         = 14
'  ADC_TEST
  #0, SLAVE_BUF_EMPTY, SLAVE_BUF_EMPTYING, SLAVE_BUF_FULL
  
CON ' ADC buffer constants
  BYTES_IN_BLOCK     =  512   ' number of bytes in each block; this is pretty well fixed
  LONGS_IN_BLOCK     =  BYTES_IN_BLOCK/4
  BLOCKS_IN_BUFFER   =  5     ' number of blocks in buffer; this depends on available memory
  LONGS_IN_BUFFER    =  LONGS_IN_BLOCK * BLOCKS_IN_BUFFER
  SLOTS_IN_BLOCK     =  30        ' how many time slices are in each buffer?

'  SAMPLES_IN_BLOCK  =  31 * 4

  BYTES_IN_SRAM      =  1024*1024/8 - 1024   ' we're using a 1 MBit device; reduce a bit to be safe
  BLOCKS_IN_SRAM     =  (BYTES_IN_SRAM / BYTES_IN_BLOCK)     
                              ' each time slice has data from 4 channels; 3-bytes for each channel
                              ' so each slice contains 12 bytes = 3-bytes * 4 channels     
                              ' a block in the buffer looks like this:
                              ' [0..3]      CNTS_AT_START_OF_BUFFER
                              ' [4..7]      channelA
                              ' [8..11]     channelB
                              ' [12..15]    channelC
                              ' [16..20]    channelD
                              ' [21..507]   DATA
                              ' [508..511]  CNTS_AT_END_OF_BUFFER
'  FSAMPC  = 1
'  FSAMPF  = 964
'  FSAMPC  FSAMPF  Coarse  Fine    Output  Divisor 20000000.00
'  0       464     4       1       10000           2000
'  0       964     4       1       8000            2500
'  5       464     8       2       5000            4000
'  5       964     8       2       4000            5000
'  4       464     16      4       2500            8000
'  4       964     16      4       2000            10000
'  3       964     32      8       1000            20000
'  2       964     64      16      500             40000
'  1       964     128     32      250             80000


CON ' UART ports and pin numbers - DEBUG, GPS, MSP
  DEBUG             =      0
  GPS_PORT          =      1
  DEBUG_BAUD        = 115200
  GPS_BAUD          =   9600

  UART_SIZE         =    100
  SPACE             =     32
  TAB               =      9
  COLON             =     58
  COMMA             =     44
  MAX_COMMANDS      =     10
  
  DEBUG_RX_FROM     = 31
  DEBUG_TX_TO       = 30
  CL = 16  ''CS: Clear Screen
  CR = 13  ''CR: Carriage Return      

CON ' main state machine states
  #0, SLEEP, WAIT_FOR_WAKE_UP, CHANGE_ACQ_SETTINGS, ACQUISITION_MODE, SHUTDOWN

  MY_TRUE  = 1
  MY_FALSE = 0

CON ' EEPROM constants
  EEPROM_BASE      = $8000             ' set to 32,768 = 32k
  
  GPB_ADDRESS      = EEPROM_BASE +  0
  SPS_ADDRESS      = EEPROM_BASE +  4
  GAIN_A_ADDRESS   = EEPROM_BASE +  8
  GAIN_B_ADDRESS   = EEPROM_BASE + 12
  GAIN_C_ADDRESS   = EEPROM_BASE + 16
  GAIN_D_ADDRESS   = EEPROM_BASE + 20
  SOURCE_A_ADDRESS = EEPROM_BASE + 24
  SOURCE_B_ADDRESS = EEPROM_BASE + 28
  SOURCE_C_ADDRESS = EEPROM_BASE + 32
  SOURCE_D_ADDRESS = EEPROM_BASE + 36

DAT 'gps header
   gpsHeader  byte   "P", "G", VERSION, 9 
DAT ' oled messages
  oledClrLine byte  "                ", 0
  oledLine1  byte   "                ", 0
  oledLine2  byte   "                ", 0
  offMsg     byte   "Shutting Down...", 0
  bootMsg    byte   "Booting System. ", 0
  sleepMsg   byte   "Sleeping System.", 0
  wakeMsg    byte   "Waking System.  ", 0
  magnetMsg  byte   "Wake with Magnet", 0
  euiMsg     byte   "                ", 0
  gpsMsg     byte   "GPS:            ", 0

OBJ                                  
  UARTS     : "FullDuplexSerial4portPlus_0v3"       '1 COG for 3 serial ports
  NUM       : "Numbers"                             'Include Numbers object for writing numbers to debuginal
  UBX       : "ubloxInterface2"
  PEBBLE    : "BasicPebbleFunctions"                ' I put these into a seperate file to make editing easier

VAR
  byte mainCogId, serialCogId, adcCogId, slaveCogId
  byte clockSetYet
  byte mainState, edgeDetector
  byte inUartBuf[UART_SIZE], inUartIdx, ptrIdx, uartState, inUartPtr[MAX_COMMANDS]
  byte oLedBuf[20] ' set aside some room for the oled 

  word year, month, day, hour, minute, second, utcValid, gpsFix
  ' ubxBufferAddress is the HUB memory location of gps data stored by ubx object
  long ubxBufferAddress
  long sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD ' these are longs because that's how they are stored in the EEPROM
  long rtcAddr        ' hub address of 7 bytes containing the RTC time
  long euiAddr        ' hub address of 48 bytes containing the unique ID for this device
  ' keep these in order  ymd, hms, cntsAtPPS are populated in the main cog but need to be passed down to the SPIslave cog
  long ymd, hms, cntsAtPPS1, cntsTemp, accuracy
  ' the next 2 line refers to the HUB storage where things sit before be shifted out to the gumstix
  long gpsDataToWrite, gpsBuffer[BYTES_IN_GPS_BUFFER]
  long slaveBufCMD, slaveBuffer[LONGS_IN_SLAVE_BUFFER]       

  ' the next line refers to data read by the ADC and stored in the HUB; this is where data rest on their way to SRAM
  long fifoSem, blocksInFIFO, adcBuffer[BLOCKS_IN_BUFFER*LONGS_IN_BLOCK] ' this is a buffer that we fill from the ADC directly. Data are
                                                                         ' moved from the ADC cog to HUB then from HUB to the PASM Slave
                                                                         ' cog where they are written out to the gumstix

PUB MAIN | idx, response, measureTime
' the main method here is a simple state machine

  adcCogId    := -1
  slaveCogId  := -1
  serialCogId := -1
  mainCogId   := cogid
  
  PEBBLE.INIT
  LAUNCH_SERIAL_COG             ' handle the 2 serial ports- debug and GPS


  PEBBLE.LED1_ON
  PEBBLE.OLED_ON
  PEBBLE.WRITE_TO_OLED(0, 1, @bootMsg)

  PAUSE_MS(2000)
  UARTS.STR(DEBUG, string(13, "$PSMSG, Booting geoPebble: "))
  PRINT_EUI
  TURN_ON_GPS

  PEBBLE.WRITE_TO_OLED(0, 1, string("Turning on GPS"))
  PAUSE_MS(2000)

  PEBBLE.WRITE_TO_OLED(0, 1, string("Loading Settings"))
  GET_AND_PRINT_PARAMETERS

  PAUSE_MS(2000)
  clockSetYet := FALSE
  
  idx := 0
  repeat 
    edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
    case edgeDetector 
      %01 : ' rising edge -> grab phsb
        idx++
        cntsTemp := CNT - PHSB
        UPDATE_TIME_AND_DATE
        PRINT_TIME_AND_DATE
        UARTS.PUTC(DEBUG, COMMA)
        UARTS.DEC(DEBUG, idx)
        PEBBLE.EXPANDER_TOGGLE_BIT(PEBBLE#EXPANDER_2,PEBBLE#LED1)
        PEBBLE.EXPANDER_TOGGLE_BIT(PEBBLE#EXPANDER_2,PEBBLE#LED2)
      %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
        PHSB := 0
      
    repeat
      response := UBX.READ_AND_PROCESS_BYTE(FALSE)
    until response == -1 OR response == UBX#RXMRAW
    
  until clockSetYet OR idx == 120

  if clockSetYet == FALSE
    UARTS.STR(DEBUG, string(13, "$PSMSG, Unable to get GPS lock or set RTC."))
  

  ' give some indication that we're booting and alive
  repeat 10 
    PAUSE_MS(150)
    PEBBLE.EXPANDER_TOGGLE_BIT(PEBBLE#EXPANDER_2,PEBBLE#LED1)
    PEBBLE.EXPANDER_TOGGLE_BIT(PEBBLE#EXPANDER_2,PEBBLE#LED2)
  
  PEBBLE.LED1_OFF
  PEBBLE.LED2_OFF
  
' setup counter A in this cog to track times when the "button" is "pressed"
' this is used to put the system back to sleep
  CTRA := %10101 << 26 + REED_SWITCH    ' counter A in LOGIC !A mode
  FRQA := 1                             ' increment phsa once for every clock cycle that REED_SWITCH is high
  PHSA := 0

  mainState := SLEEP
  uartState := WAITING_FOR_START


  UARTS.STR(DEBUG, string(13, "$PSMSG. Sleeping system.  Use magnet to wake."))
  PEBBLE.WRITE_TO_OLED(0, 1, @sleepMsg)
  PEBBLE.WRITE_TO_OLED(0, 2, @magnetMsg)
  PAUSE_MS(3000)
  

  repeat
    if PHSA > CONSTANT(100_000_000*10)  ' has button been pressed for more than 10 seconds?
      UARTS.STR(DEBUG, string(13, "$PSMSG, Rebooting system"))
      PEBBLE.WRITE_TO_OLED(0, 1, string("REBOOTING"))
      PEBBLE.WRITE_TO_OLED(0, 1, string("SYSTEM"))
      PAUSE_MS(2_000)
      REBOOT
      
    if (PHSA > BUTTON_HOLD_TIME) AND INA[REED_SWITCH]==NOT_PRESSED
      mainState := SHUTDOWN
      PHSA := 0
                              
    case mainState
      SLEEP             :
        PAUSE_MS(200)
        PEBBLE.SLEEP(cogId)
        mainState := WAIT_FOR_WAKE_UP
                               
      WAIT_FOR_WAKE_UP :
        if PEBBLE.SWITCHED_ON
          PEBBLE.WAKE_UP
          LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
          PAUSE_MS(200)
          UARTS.STR(DEBUG, string(13, "$PSMSG, System booted and ready for action."))
          PEBBLE.WRITE_TO_OLED(0, 1, @bootMsg)
          START_ACQUISITION
          mainState := ACQUISITION_MODE
          PHSA      := 0
        else
          mainState := WAIT_FOR_WAKE_UP
          PAUSE_MS(10)

{      WAIT_FOR_WAKE_UP  :
        PEBBLE.WAIT_FOR_WAKE_UP               ' this calls forces system to block until button pressed
        LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
        PAUSE_MS(200)
        UARTS.STR(DEBUG, string(13, "System booted and ready for action.",13))
        PEBBLE.WRITE_TO_OLED(0, 1, @bootMsg)
        START_ACQUISITION
        mainState := ACQUISITION_MODE
        PHSA      := 0
}

      ACQUISITION_MODE  :
        DO_SOMETHING_USEFUL

      SHUTDOWN          :
        PEBBLE.  WRITE_TO_OLED(0, 1, string("SHUTTING DOWN   "))
        UARTS.STR(DEBUG, string(13, "$PSMSG, Shutting down...."))
        PAUSE_MS(1000)
        mainState := SLEEP

PUB START_ACQUISITION 
' after waking from sleep we need to start up a number of different things.
' everything that needs to be done only once/start should go here.
' all of this happens in the top-level cog

  UARTS.STR(DEBUG, string(13, "$PSMSG, Powering Analog Section."))  
  PEBBLE.ANALOG_ON
  UARTS.STR(DEBUG, string(13, "$PSMSG, Powering GumStix."))
  PEBBLE.GUMSTIX_ON
  TURN_ON_GPS
  fifoSem   := locknew  'semaphore that guards the blocksInFIFO HUB location which is shared by 2 PASM cogs
  gpsSem    := locknew
  

  gpsDataToWrite := FALSE            ' remember FALSE = 0
  gpsFlagPtr     := @gpsDataToWrite  ' pass the HUB address to SPI_SLAVE cog
  gpsBasePtr     := @gpsBuffer       ' pass the HUB address to SPI_SLAVE cog
  timeDatePtr    := @ymd
  blocksInFIFO   := 0

  GET_PARAMETERS_FROM_EEPROM
  
  PRINT_SYSTEM_PARAMETERS
  
  slaveCogId := cognew(@GUMSTIX_SLAVE, @fifoSem)        ' Start the SPI slave cog first and move stuff from HUB to Gumstix
  UARTS.STR(DEBUG, string(13, "$PSMSG, GUMSITX_SLAVE: "))
  UARTS.DEC(DEBUG, slaveCogId)

  PAUSE_MS(1000)
  adcCogId := SETUP_AND_LAUNCH_ADC(sampleRate, gainA, gainB, gainC, gainD, sourceA, sourceB, sourceC, sourceD)
  UARTS.STR(DEBUG, string(13, "$PSMSG, ADC_COG_ID: "))
  UARTS.DEC(DEBUG, adcCogId)

  'UARTS.STR(DEBUG, string(13, "SPI_MASTER: "))
  'UARTS.DEC(DEBUG, cognew(@SPI_MASTER, 0))' SPI MASTER code used for testing only; be sure to start the SPI slave cog first
  


  CTRB[30..26] := %01000                                'ctrb module to POS detector;                                                        
  CTRB[5..0]   := GPS_PPS                               'add frqb to phsb every clock cycle that GPS_PPS high
  FRQB:= 1                                              'this will keep track of the number of system counts
  PHSB~                                                 'from the rising edge of GPS

PUB GET_AND_PRINT_PARAMETERS
  ' check to see if factury defaults have already been stored
  CHECK_DEFAULT_PARAMS
  GET_PARAMETERS_FROM_EEPROM
  PRINT_SYSTEM_PARAMETERS                                                     

PUB DO_SOMETHING_USEFUL | rxByte, response, idx
' put things here that need to be done but have some timing flexibility
' all of this happens in the top level cog

  ' here we collect bytes from uart and process them.
  ' this should follow the same setup as the ublox I think
   rxByte := uarts.rxcheck(DEBUG)       ' collect byte from DEBUG port
   case uartState
      WAITING_FOR_START      :   if rxByte == "$"                     ' beginning of a new string
                                   inUartIdx := 0
                                   ptrIdx    := 0
                                   bytefill(@inUartIdx, 0, UART_SIZE) ' buffer for all incoming bytes
                                   bytefill(@inUartPtr, 0, MAX_COMMANDS) ' contains the index of each field 
                                   uartState := WAITING_FOR_END

      WAITING_FOR_END        :   if rxByte <> -1
                                   case rxByte
                                     ","    :
                                       inUartBuf[inUartIdx++ <# UART_SIZE] := 0          ' zero terminate strings
                                       inUartPtr[++ptrIdx] := inUartIdx <# MAX_COMMANDS
                                       uartState := WAITING_FOR_END
                                     "%"   :
                                       uartState := PROCESS_BUFFER
                                     OTHER : 
                                       inUartBuf[inUartIdx++ <# UART_SIZE] := rxByte     ' if not, keep adding to it
                                       uartState := WAITING_FOR_END

     PROCESS_BUFFER          :   PROCESS_UART
                                 uartState := WAITING_FOR_START
    

     
  edgeDetector := ((edgeDetector << 1) | INA[GPS_PPS] ) & %11    
  case edgeDetector 
    %01 : ' rising edge -> grab phsb
      cntsTemp := CNT - PHSB
      UPDATE_TIME_AND_DATE                   ' KEEP IN MIND WE ALWAYS UPDATE TIME/DATE ON THE PPS following OUR MESSAGE SO WE ARE BEHIND ONE SECOND
      PRINT_TIME_AND_DATE
    %10 : ' falling edge now we can reset phsb since it won't increment until PPS goes high again
      PHSB := 0
      
  repeat
    response := UBX.READ_AND_PROCESS_BYTE(FALSE)
  until response == -1 OR response == UBX#RXMRAW
  
  if response == UBX#RXMRAW                             ' have we collected all the data in this second?
    'UARTS.STR(DEBUG, string(13,"$PSMSG, FIFO: "))
     'UARTS.DEC(DEBUG, blocksInFIFO)                    ' if the gps buffer is empty put something into it
    if gpsDataToWrite == MY_FALSE                      ' have the data we put there previously been written?
      longmove(@gpsBuffer, ubxBufferAddress, 256)      ' copy that info into this cog
      longmove(@gpsBuffer[129], @gpsBuffer[127], 126) ' move data over so we can insert header
      longmove(@gpsBuffer[1],   @gpsBuffer[0],   127) ' move data over so we can insert header
      longmove(@gpsBuffer[128], @gpsHeader, 1)          ' insert 4-byte gps header
      longmove(@gpsBuffer[0],   @gpsHeader, 1)          ' insert 4-byte gps header

    ' buffer has new data so clear old buffer and declare buffer should be written
    UBX.CLEAR_UBX_BUFFER                                ' clear the pointer on that side so it can process next second
    'longfill(@gpsBuffer, 0, 256)
    gpsDataToWrite := MY_TRUE                           ' let the other cogs know there are gps data to write

  return

PUB UPDATE_TIME_AND_DATE | isLeapYear
'' update the local copy of year, month, day, hour, minute, second

  year     :=  UBX.YEAR
  month    :=  UBX.MONTH
  day      :=  UBX.DAY
  hour     :=  UBX.HOUR
  minute   :=  UBX.MINUTE
  second   :=  UBX.SECOND
  utcValid := UBX.UTC_TIME_VALID
  gpsFix   := UBX.GET_STATUS_FIX
  accuracy := UBX.ACCURACY_NS

  isLeapYear := 1 + ((year//4 == 0 AND year//100 <> 0 OR year//400 == 0)) ' check to see if this is a leap year
  ' leap = 0 means leap year; leap = 1 means not a leap year

  second += 1
  if second == 60
    second := 0
    minute++
  if minute == 60
    minute := 0
    hour++
  if hour == 24
    hour := 0
    day++
    if isLeapYear
      if day > leapTable[month-1]
        day := 0
        month++
        if month > 12
          month := 1
          year++
    else  
      if day > noLeapTable[month-1]
        day := 0
        month++
        if month > 12
          month := 1
          year++

  
  repeat until not lockset(gpsSem)       ' wait here until we can get the semaphore
    UARTS.STR(DEBUG,string(13,"Waiting for gpsSem."))
    PAUSE_MS(10)
  ymd    := year * 10_000 + month * 100 + day
  hms    := hour * 10_000 + minute * 100 + second 
  cntsAtPPS1 := cntsTemp
  lockclr(gpsSem)
  
PUB PRINT_TIME_AND_DATE

'  uarts.str(debug, string(13,"DATE:"))
  oLedBuf[0] := " "
  oLedBuf[1] := " "
  oLedBuf[2] := " "
  oLedBuf[3] := year  / 1000 + "0"
  oLedBuf[4] := year  / 100 // 10 + "0"
  oLedBuf[5] := year  / 10 // 10+ "0"
  oLedBuf[6] := year // 10 + "0"
  oLedBuf[7] := "-"
  oLedBuf[8] := month  / 10 + "0"
  oLedBuf[9] := month // 10+ "0"
  oLedBuf[10] := "-"
  oLedBuf[11] := day  / 10 + "0"
  oLedBuf[12] := day // 10+ "0"
  oLedBuf[13] := " "
  oLedBuf[14] := " "
  oLedBuf[15] := " "
  oLedBuf[16] := 0              ' zero terminate the buffer
'  uarts.str(debug, @oLedBuf)  
'  PEBBLE.WRITE_TO_OLED(0, 1, @oLedBuf)

'  uarts.str(debug, string(13,"TIME:"))
  oLedBuf[0] := " "
  oLedBuf[1] := " "
  oLedBuf[2] := " "
  oLedBuf[3] := " "
  oLedBuf[4] := hour / 10 + "0"
  oLedBuf[5] := hour // 10 + "0"
  oLedBuf[6] := ":"
  oLedBuf[7] := minute / 10 + "0"
  oLedBuf[8] := minute // 10 + "0"
  oLedBuf[9] := ":"
  oLedBuf[10] := second / 10 + "0"
  oLedBuf[11] := second // 10 + "0"
  oLedBuf[12] := " "
  oLedBuf[13] := " "
  oLedBuf[14] := " "
  oLedBuf[15] := " "
  oLedBuf[16] := 0
'  uarts.str(debug, @oLedBuf)  
'  PEBBLE.WRITE_TO_OLED(0, 2, @oLedBuf)

  uarts.str(debug, string(13, "$PSSTAT,"))
  uarts.dec(debug, year)
  uarts.putc(debug, "-")
  uarts.dec(debug, month)
  uarts.putc(debug, "-")
  uarts.dec(debug, day)
  uarts.putc(debug, "T")
  uarts.dec(debug, hour)
  uarts.putc(debug, ":")
  uarts.dec(debug, minute)
  uarts.putc(debug, ":")
  uarts.dec(debug, second)
  uarts.putc(debug, ",")
  uarts.dec(debug, cntsAtPPS1)
  uarts.putc(debug, ",")
  uarts.dec(debug, accuracy)
  uarts.putc(debug, ",")
  uarts.bin(debug, utcValid, 3)
  uarts.putc(debug, ",")
  uarts.bin(debug, gpsFix, 6)
  uarts.putc(debug, ",")
  if ((gpsFix & %0011_1111) > 1)
    uarts.STR(DEBUG, string("GPSFIX"))
  else
    uarts.STR(DEBUG, string("INVALID"))
  uarts.putc(debug, ",")
  uarts.dec(debug, UBX.get_longitude)
  uarts.putc(debug, ",")
  uarts.dec(debug, UBX.get_latitude)
  uarts.putc(debug, ",")
  uarts.dec(debug, UBX.get_elevation)

  if clockSetYet == FALSE AND ((gpsFix & %0011_1111) > 1) 
    uarts.STR(DEBUG, string(13,"Setting RTC: "))
    PEBBLE.SET_RTC_TIME(year, month, day, hour, minute, second, 0)
    READ_RTC_TIME
    PRINT_RTC_TIME
    clockSetYet := TRUE


'  if second == 4
  PEBBLE.WRITE_TO_OLED(0, 1, @bootMsg)
'      PEBBLE.WRITE_TO_OLED(5, 1, NUM.TOSTR(month, NUM#DEC2))
'      PEBBLE.WRITE_TO_OLED(7, 1, NUM.TOSTR(day,   NUM#DEC2))
    

PUB SETUP_AND_LAUNCH_ADC(_sps, gA, gB, gC, gD, aMux, bMux, cMux, dMux) | sampleRateEnum, response
'' store system wide values for sps, gain, and internal/external 1 for external; 0 for internal
'' set the gain and voltage trim (0)
'' set sample rate and launch ADC cog
'' method that loads some variables and launches the ADC cog

  if adcCogId > 0   ' is the ADC cog already running
    cogstop(adcCogId)
    adcCogId := -1

                                                                           
  sampleRateEnum := lookdown(_sps:   250, 500, 1000, 2000, 2500, 4000, 5000, 8000, 10000)
  fsampc   := lookup(sampleRateEnum:   1,   2,    3,    4,    4,    5,    5,    0,     0)                      
  fsampf   := lookup(sampleRateEnum: 964, 964,  964,  964,  464,  964,  464,  964,   464)                      

  wdrcrVal := (FSAMPC << 13) | FSAMPF  ' write data rate control register value
  rdrcrVal := 0                        ' read data rate control register value
  wsicrVal := $0000_0000               ' write sampling instant control register value
  rsicrVal := 0                        ' read sampling instant contrl register value
  wcrVal   := %0_0_1_0_0_0_00          ' write configuration register
  rcrVal   := 0                        ' read configuration register

  gainA := PEBBLE.SET_GAIN(gA, CHANNEL_A)
  gainB := PEBBLE.SET_GAIN(gB, CHANNEL_B)
  gainC := PEBBLE.SET_GAIN(gC, CHANNEL_C)
  gainD := PEBBLE.SET_GAIN(gC, CHANNEL_D)

'  PEBBLE.SHORT_INPUTS(CHANNEL_A)
'  PEBBLE.SHORT_INPUTS(CHANNEL_B)
'  PEBBLE.SHORT_INPUTS(CHANNEL_C)
'  PEBBLE.SHORT_INPUTS(CHANNEL_D)

  PEBBLE.SET_MUXES(aMux, bMux, cMux, dMux)

  ' encode the upper-most 4 bits with the channel code and
  ' the next 4 bits with gain.  This will be present in every sample
  codeA    := 0'((CHANNEL_A << 4) | gA) << 24
  codeB    := 0'((CHANNEL_B << 4) | gB) << 24
  codeC    := 0'((CHANNEL_C << 4) | gC) << 24
  codeD    := 0'((CHANNEL_D << 4) | gD) << 24
   
  data1.byte[3] := "P"
  data1.byte[2] := "S"
  data1.byte[1] := VERSION
  data1.byte[0] := 9 ' I need to figure out how to get the log2 value here
  
  data2.byte[3] := SLOTS_IN_BLOCK & $FF
  data2.byte[2] := sampleRateEnum
  data2.byte[1] := %0000_1111   ' high bit for each active channel
  data2.byte[0] := 0    ' this needs to be set to clock lock somehow! FIXME


  bytefill(@adcBuffer, 0, LONGS_IN_BUFFER*4)
  adcCogId := cognew(@MAX11040K, @fifoSem)        ' Start the ADC read cog

  return adcCogId

PUB PROCESS_UART | idx
' put the code necessary to process incoming UART commands here
{
    KILL                  = "K"
    SHUTDOWN_SYSTEM       = "D"
    SOFT_REBOOT_GUMSTIX   = "S"
    HARD_REBOOT_GUMSTIX   = "H"
    PUT_SYSTEM_TO_SLEEP   = "L"
    RATE                  = "R"
    QUERY                 = "Q"
    GAIN                  = "G"
    SOURCE                = "O"
    WRITE_TO_OLED         = "W"
}
  
'  UARTS.STR(DEBUG, string(13,"Received command: "))
'  UARTS.PUTC(DEBUG, inUartBuf[0])

  
'  UARTS.STR(DEBUG, string(13,"inUartBuf: "))
'  repeat idx from 0 to 20
'    UARTS.HEX(DEBUG, inUartBuf[idx],2)
'    UARTS.PUTC(DEBUG, SPACE)
    
'  UARTS.STR(DEBUG, string(13,"inUartPtr: "))
'  repeat idx from 0 to 5
'    UARTS.HEX(DEBUG, inUartPtr[idx], 2)
'    UARTS.PUTC(DEBUG, SPACE)

'  UARTS.PUTC(DEBUG, CR)
  
  case inUartBuf[0]
    KILL :                 'K
      UARTS.STR(DEBUG, string(13,"$PSCMD, KILL"))
      PEBBLE.GUMSTIX_OFF

    SHUTDOWN_SYSTEM :      'D
      UARTS.STR(DEBUG, string(13,"$PSCMD, SHUTDOWN"))
      PAUSE_MS(12_000)
      mainState := SHUTDOWN

    SOFT_REBOOT_GUMSTIX :  'S
      UARTS.STR(DEBUG, string(13,"$PSCMD, REBOOT"))
      PAUSE_MS(2000)

    HARD_REBOOT_GUMSTIX :  'H
      PEBBLE.GUMSTIX_OFF
      PAUSE_MS(500)
      PEBBLE.GUMSTIX_ON

    PUT_SYSTEM_TO_SLEEP :  'L
      PEBBLE.GUMSTIX_OFF
      PAUSE_MS(500)
      PEBBLE.GUMSTIX_ON

    RATE :                 'R
      UARTS.STR(DEBUG, string(" Changing sample rate to: "))
      sampleRate := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      UARTS.DEC(DEBUG, sampleRate)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_EEPROM
      START_ACQUISITION

    QUERY :                ' Q
      PRINT_SYSTEM_PARAMETERS
      
    GAIN :                 'G
      UARTS.STR(DEBUG, string(" Changing gain to: "))
      gainA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      gainB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC) 
      gainC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC) 
      gainD := NUM.FROMSTR(@inUartBuf[inUartPtr[4]],NUM#DDEC) 
      UARTS.DEC(DEBUG, gainA)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainB)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainC)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, gainD)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_EEPROM
      START_ACQUISITION
      
    SOURCE :               'O
      UARTS.STR(DEBUG, string(" Changing input source to: "))
      sourceA := NUM.FROMSTR(@inUartBuf[inUartPtr[1]],NUM#DDEC) 
      sourceB := NUM.FROMSTR(@inUartBuf[inUartPtr[2]],NUM#DDEC) 
      sourceC := NUM.FROMSTR(@inUartBuf[inUartPtr[3]],NUM#DDEC) 
      sourceD := NUM.FROMSTR(@inUartBuf[inUartPtr[4]],NUM#DDEC) 
      UARTS.DEC(DEBUG, sourceA)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceB)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceC)
      UARTS.PUTC(DEBUG, TAB)
      UARTS.DEC(DEBUG, sourceD)
      UARTS.STR(DEBUG, string(13,"Stopping adcCog: "))
      UARTS.DEC(DEBUG, adcCogId)
      UARTS.STR(DEBUG, string(13,"Stopping slaveCog: "))
      UARTS.DEC(DEBUG, slaveCogId)
      cogStop(adcCogId)
      cogStop(slaveCogId)
      STORE_PARAMETERS_TO_EEPROM
      START_ACQUISITION

    WRITE_TO_OLED :        'W
      'UARTS.STR(DEBUG, string(" Write to OLED: "))
      'UARTS.STR(DEBUG, @inUartBuf[2])
      'UARTS.PUTC(DEBUG, CR)
      PEBBLE.WRITE_TO_OLED(0, 2, @oledClrLine)
      PAUSE_MS(25)
      PEBBLE.WRITE_TO_OLED(0, 2, @inUartBuf[2])
      PAUSE_MS(25)
       
    OTHER: 
      UARTS.STR(DEBUG, string(" Unrecognized command."))

PUB PRINT_EUI
  bytefill(@euiAddr, 0, 8)
  euiAddr := PEBBLE.READ_EUI + 3' skip the first 3 bytes
  euiMsg[0] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[1] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  euiMsg[2] := ":"
  euiMsg[3] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[4] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  euiMsg[5] := ":"
  euiMsg[6] := PEBBLE.TO_HEX((BYTE[euiAddr] >> 4 & $F))
  euiMsg[7] := PEBBLE.TO_HEX((BYTE[euiAddr++] & $F))
  
  
  PEBBLE.WRITE_TO_OLED(0,1,@euiMsg)
  UARTS.STR(DEBUG, @euiMsg)

PUB READ_RTC_TIME
  rtcAddr:= PEBBLE.READ_RTC_TIME

PUB PRINT_RTC_TIME
  UARTS.STR(DEBUG, string(13,"$PSMSG, RTC Date and time: "))
  UARTS.DEC(DEBUG, BYTE[rtcAddr][6]+2000)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][5])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][4])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][3])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][2])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][1])
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, BYTE[rtcAddr][0])
  UARTS.PUTC(DEBUG, SPACE)

PUB GET_PARAMETERS_FROM_EEPROM
  sampleRate :=  PEBBLE.READ_EEPROM_LONG(SPS_ADDRESS) 
  gainA      :=  PEBBLE.READ_EEPROM_LONG(GAIN_A_ADDRESS)
  gainB      :=  PEBBLE.READ_EEPROM_LONG(GAIN_B_ADDRESS)
  gainC      :=  PEBBLE.READ_EEPROM_LONG(GAIN_C_ADDRESS)
  gainD      :=  PEBBLE.READ_EEPROM_LONG(GAIN_D_ADDRESS)
  sourceA    :=  PEBBLE.READ_EEPROM_LONG(SOURCE_A_ADDRESS)
  sourceB    :=  PEBBLE.READ_EEPROM_LONG(SOURCE_B_ADDRESS)
  sourceC    :=  PEBBLE.READ_EEPROM_LONG(SOURCE_C_ADDRESS)
  sourceD    :=  PEBBLE.READ_EEPROM_LONG(SOURCE_D_ADDRESS)

PUB STORE_PARAMETERS_TO_EEPROM
  PEBBLE.WRITE_EEPROM_LONG(SPS_ADDRESS, sampleRate)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_A_ADDRESS, gainA)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_B_ADDRESS, gainB)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_C_ADDRESS, gainC)
  PEBBLE.WRITE_EEPROM_LONG(GAIN_D_ADDRESS, gainD)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_A_ADDRESS, sourceA)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_B_ADDRESS, sourceB)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_C_ADDRESS, sourceC)
  PEBBLE.WRITE_EEPROM_LONG(SOURCE_D_ADDRESS, sourceD)
  
PUB CHECK_DEFAULT_PARAMS | eepromValue

  eepromValue := PEBBLE.READ_EEPROM_LONG(GPB_ADDRESS)
  if eepromValue == $DEAD_BEEF
    UARTS.STR(DEBUG, string(13,"$PSMSG, Factory parameters already set."))
  else
    WRITE_DEFAULT_PARAMS_TO_EEPROM

PUB WRITE_DEFAULT_PARAMS_TO_EEPROM
  sampleRate := 1000
  gainA      :=    1
  gainB      :=    1
  gainC      :=    1
  gainD      :=   10
  sourceA    := INTERNAL
  sourceB    := INTERNAL
  sourceC    := INTERNAL
  sourceD    := INTERNAL
  STORE_PARAMETERS_TO_EEPROM
  PEBBLE.WRITE_EEPROM_LONG(GPB_ADDRESS, $DEAD_BEEF)
  UARTS.STR(DEBUG, string(13,"$PSMSG, Default paramaters stored to EEPROM."))
  PEBBLE.WRITE_TO_OLED(0, 1, string("WROTE TO EEPROM "))
  PAUSE_MS(1500)

PUB PRINT_SYSTEM_PARAMETERS
  
  UARTS.STR(DEBUG, string(13, "$PSPARMS: "))
  UARTS.DEC(DEBUG, sampleRate)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainA)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainB)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainC)
  UARTS.PUTC(DEBUG, COMMA)
  UARTS.DEC(DEBUG, gainD)
  UARTS.PUTC(DEBUG, COMMA)
  if sourceA == INTERNAL
    UARTS.STR(DEBUG, string(" INTERNAL"))
  if sourceA == EXTERNAL
    UARTS.STR(DEBUG, string(" EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceB == INTERNAL
    UARTS.STR(DEBUG, string(" INTERNAL"))
  if sourceB == EXTERNAL
    UARTS.STR(DEBUG, string(" EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceC == INTERNAL
    UARTS.STR(DEBUG, string(" INTERNAL"))
  if sourceC == EXTERNAL
    UARTS.STR(DEBUG, string(" EXTERNAL"))
  UARTS.PUTC(DEBUG, COMMA)
  if sourceD == INTERNAL
    UARTS.STR(DEBUG, string(" INTERNAL"))
  if sourceD == EXTERNAL
    UARTS.STR(DEBUG, string(" EXTERNAL"))

  UARTS.PUTC(DEBUG, CR)

     
PUB STOP_ADC_COG
'' method that kills off the ADC cog
  lockret(fifoSem)                               ' 
  cogstop(adcCogId)
  adcCogId := -1

PUB LAUNCH_SERIAL_COG
'' method that sets up the serial ports
  NUM.INIT
  UARTS.INIT
  UARTS.ADDPORT(DEBUG,    DEBUG_RX_FROM, DEBUG_TX_TO, -1, -1, 0, %000000, DEBUG_BAUD)    'Add DEBUG port
  UARTS.ADDPORT(GPS_PORT, GPS_RX_FROM,   GPS_TX_TO,   -1, -1, 0, %000000, GPS_BAUD)      'Add condor NMEA port
  UARTS.START
  serialCogId    := UARTS.GETCOGID                                               'Start the ports
  PAUSE_MS(200)

PUB TURN_ON_GPS
  UARTS.STR(DEBUG, string(13, "$PSMSG, GPS ON."))
  PEBBLE.GPS_ON
  ubxBufferAddress := UBX.INIT(DEBUG, GPS_PORT)          

  ' we need some amount of delay between turning on the gps and speaking to it.  
  PAUSE_MS(100)
  UBX.VERSION_POLL
  UBX.TURN_OFF_NMEA
  UBX.TURN_ON_RAW
  UBX.TURN_ON_PPS


PUB TEST_MAG_ACC | idx, measureTime
  PEBBLE.MAG_ACC_ON
  PEBBLE.LSM_INIT
  
  idx := 0
  measureTime := cnt + clkfreq
  repeat
    UARTS.PUTC(DEBUG, 13)
    waitcnt(measureTime +=  CONSTANT(100_000_000/25))
    'if idx // 20 == 0
    '  UARTS.STR(DEBUG, string(13, "ACCx",9,"ACCy",9,"ACCz",9,"MAGx",9,"MAGy",9,"MAGz",9,"TEMP",13))
    'idx++
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_X)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_Y)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)
    UARTS.DEC(DEBUG, PEBBLE.GET_ACC_Z)
    UARTS.PUTC(DEBUG, TAB)
'    PAUSE_MS(1)

    PEBBLE.READ_MAG             ' we need to read the mag before the others are populated.  
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_X)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_Y)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.GET_MAG_Z)
    UARTS.PUTC(DEBUG, TAB)
    UARTS.DEC(DEBUG, PEBBLE.READ_MAG_TEMP)

PUB TEST_RTC | setDow, setYear, setMon, setDay, setHour, setMin, setSec

  setDow  :=    4
  setYear := 2014
  setMon  :=   11
  setDay  :=    6
  setHour :=   21
  setMin  :=   46
  setSec  :=   30

  PEBBLE.SET_RTC_TIME(setYear, setMon, setDay, setHour, setMin, setSec, setDow)
  UARTS.STR(DEBUG, string(13,"Setting RTC time: "))
  UARTS.DEC(DEBUG, setYear)
  UARTS.PUTC(DEBUG, "-")
  UARTS.DEC(DEBUG, setMon)
  UARTS.PUTC(DEBUG, "-")
  UARTS.DEC(DEBUG, setDay)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, setHour)
  UARTS.PUTC(DEBUG, COLON)
  UARTS.DEC(DEBUG, setMin)
  UARTS.PUTC(DEBUG, COLON)
  UARTS.DEC(DEBUG, setSec)
  UARTS.PUTC(DEBUG, SPACE)
  UARTS.DEC(DEBUG, setDow)

  repeat
    UARTS.STR(DEBUG, string(13, "Going to sleep.",13))
    PAUSE_MS(500)
    PEBBLE.SLEEP(mainCogId)
    PEBBLE.WAIT_FOR_TIME(2)
    LAUNCH_SERIAL_COG                     ' handle the 2 serial ports- debug, and GPS
    PAUSE_MS(200)
    UARTS.STR(DEBUG, string(13, "System booted and ready for action.",13))
    PEBBLE.WRITE_TO_OLED(0, 1, @bootMsg)
    repeat 
      rtcAddr := PEBBLE.READ_RTC_TIME
      UARTS.STR(DEBUG, string(13,"Date and time: "))
      UARTS.DEC(DEBUG, BYTE[rtcAddr][6]+2000)
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][5])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][4])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][3])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][2])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][1])
      UARTS.PUTC(DEBUG, SPACE)
      UARTS.DEC(DEBUG, BYTE[rtcAddr][0])
      UARTS.PUTC(DEBUG, SPACE)
      PAUSE_MS(500)
    until BYTE[rtcAddr][0] == 15    
  

  
PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

PUB PAUSE_US(uS)
  waitcnt(clkfreq/1_000_000 * uS + cnt)

DAT ' day in each month for leap and non-leap year
  leapTable   byte  0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' leap year
  noLeapTable byte  0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30  ' NOT leap year

DAT 'GUMSTIX_SLAVE
'' this PASM code should watch the shared HUB memory ADC buffers and write each buffer full
'' as soon as it is available IF the gumstix is ready

GUMSTIX_SLAVE         org            0
          mov     slaveSemPtr,   par
          mov     fifoCntPtr,    par                    ' this points to the number of blocks in the FIFO?
          mov     slaveBasePtr,  par                    ' base address of the data
          add     fifoCntPtr,    #4
          add     slaveBasePtr,  #8

          
          rdlong  slaveSem,      slaveSemPtr            ' get a copy of the semaphore address
          call    #SETUP_SLAVE_PINS                     ' setup pins
          

:topOfBuffer
          mov     bufferCnt,     #BLOCKS_IN_BUFFER      ' keep track of how many blocks are in the entire buffer
          mov     slavePtr,      slaveBasePtr           ' make sure pointer to next sample is at the beginning of entire buffer

          
:waitForData    ' wait here until the ADC or GPS buffer has at least one block in it
          rdlong  stemp,         gpsFlagPtr         wz   ' stemp=1 means data to write
   if_nz  call    #WRITE_GPS_DATA                        ' Z=1 if stemp=0
          rdlong  fifoCnt,       fifoCntPtr         wz   ' Z=1 if fifoCnt=0
   if_z   jmp     #:waitForData                          ' if Z==0 jump back up and wait some more


' at this point we've got at least one block we can send
          mov     sampsInBlock,  #LONGS_IN_BLOCK wz    ' number of times we should loop per block; set Z=0
          
          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW

          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:slaveLoop
          'now get ready to shift out all 128-longs
        
          rdlong  soutData,      slavePtr              ' get long from HUB
          'mov     soutData,      sixsix                ' put 0x5858_5858 into outbyte
          call    #WRITE_LONG
          add     slavePtr,      #4                    ' increment bufferPtr
          
          djnz    sampsInBlock,  #:slaveLoop
          call    #UPDATE_FIFO_CNT
          
          djnz    bufferCnt,     #:waitForData   wz     ' jump back around; when count=0 set Z=1
          

          jmp     #:topOfBuffer                        ' jump up and wait for next buffer



{******************************** WRITE_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
WRITE_LONG
          mov     sBits,        #32        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero

:wslaveLoop
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     soutData,     #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:wslaveLoop     ' Decrement loop count.

          muxz    outa,         somiMask         ' make sure MISO is low on exit

WRITE_LONG_ret    ret


{******************************** SETUP_SLAVE_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_SLAVE_PINS

' Setup MISO pin: Preset low; set to output
          mov     stemp,         #1           wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          somiMask                ' Preset OUTA to low         "0"
          muxnz   dira,          somiMask               ' Set    DIRA to HIGH=OUTPUT "1"


' Setup SLAVE_IRQ pin: Preset high; set to output
          mov     stemp,         #1           wz         ' Z=0;
          muxnz   outa,          irqMask                 ' Preset OUTA to HIGH        "1"
          muxnz   dira,          irqMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup TESTPT pin: Preset low; set to output
'          mov     stemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          stestMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          stestMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_SLAVE_PINS_ret    ret

{******************************** UPDATE_FIFO_CNT*****************************************************
Use this snippet to update the HUB long which contains the number of elements in the FIFO.
This needs a semaphore because this update involves a read, modify, write.  The same procedure
is used on the ADC cog.  
***************************************************************************************************}
UPDATE_FIFO_CNT
:waitForSlaveSem
          cmp     szero,          #0           wz         ' set Z=1 
'          xor     outa,          stestMask

          lockset slaveSem        wc
   if_c   jmp     #:waitForSlaveSem                     ' if adcSem = 0 loop 

          rdlong  stemp,         fifoCntPtr              ' copy number of blocks in buffer so we can add to it
          sub     stemp,         #1                      ' increment count
          wrlong  stemp,         fifoCntPtr              ' write the new value back to HUB
          lockclr slaveSem                              ' clear adcSem for others to use
'          xor     outa,          stestMask

UPDATE_FIFO_CNT_ret  ret

{******************************** WRITE_GPS_DATA*****************************************************
This method pulls GPS dat from HUB and spits it to the shared SPI_BUS
***************************************************************************************************}
WRITE_GPS_DATA
' we've got 1024 bytes or 256 longs of gps data to send.  So we'll do it in two parts.  One then the next
          mov     gpsPtr,        gpsBasePtr             ' we always begin at the beginning of the GPS buffer
          mov     gpsBufCnt,     #2                     ' we need to send 2 buffers

:gpsLoop2
          mov     gpsInBlock,    #LONGS_IN_BLOCK wz     ' number of times we should loop per block; set Z=0
          
          waitpeq scsMask,       scsMask                ' wait for CS to be HIGH
          muxz    outa,          irqMask                ' lower controller FLAG to indicate buffer ready to be clocked out
          waitpne scsMask,       scsMask                ' wait for CS to be LOW

          muxnz   outa,          irqMask                ' raise controller FLAG once CS goes low

:gpsLoop1
          'now shift out all 128-longs in current block

          rdlong  gpsLong,       gpsPtr                 ' get long from HUB
          'mov     soutData,      sixsix
          call    #WRITE_GPS_LONG
          add     gpsPtr,        #4                     ' increment bufferPtr
          
          djnz    gpsInBlock,    #:gpsLoop1
          
          djnz    gpsBufCnt,     #:gpsLoop2

          wrbyte  sfalse,            gpsFlagPtr             ' indicate the GPS buffer if empty

WRITE_GPS_DATA_ret  ret

{******************************** WRITE_GPS_LONG *******************************************************
Write 32 bits to gumstix.  
***************************************************************************************************}
WRITE_GPS_LONG
          mov     sBits,        #8        wz   ' setup number of bits in write; set Z=0 
          mov     stemp,        gpsLong
          shl     gpsLong,      #24
          
:loop1
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop1          ' Decrement loop count.

          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          shl     gpsLong,      #16
          
:loop2
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop2          ' Decrement loop count.
                                                 
          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          shl     gpsLong,      #8
          
:loop3
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop3          ' Decrement loop count.

          mov     sBits,        #8        wz     ' setup number of bits in write; set Z=0 
          mov     gpsLong,      stemp
          
:loop4
          waitpne ssclkMask,    ssclkMask        ' wait for CLK to go low
          rol     gpsLong,      #1         wc    ' get highest bit of outData
          muxc    outa,         somiMask         ' put bit onto MISO
          waitpeq ssclkMask,    ssclkMask        ' wait for CLK to go high
          djnz    sBits,        #:loop4          ' Decrement loop count.

          muxz    outa,         somiMask         ' make sure MISO is low on exit

WRITE_GPS_LONG_ret  ret


{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

scsMask      long            |< SLAVE_CS               '
ssclkMask    long            |< SLAVE_CLK
somiMask     long            |< SLAVE_SOMI
simoMask     long            |< SLAVE_SIMO
irqMask      long            |< SLAVE_IRQ 
'stestMask    long            |< SLAVE_TEST

fiveEight    long            $5858_5858
sixsix       long            $6666_6666
gpsFlagPtr   long            0
gpsBasePtr   long            0
gpsPtr       long            0
gpsBufCnt    long            0
gpsInBlock   long            0
sampsInBlock long            0
szero        long            0
byte1Mask    long            $FF00_0000
byte2Mask    long            $00FF_0000
byte3Mask    long            $0000_FF00
byte4Mask    long            $0000_00FF
sfalse       long            0

byte1            res             1
byte2            res             1
byte3            res             1
byte4            res             1
gpsLong          res             1
slaveSemPtr      res             1 
slaveSem         res             1
fifoCntPtr       res             1
fifoCnt          res             1
slaveBasePtr     res             1
slavePtr         res             1
bufferCnt        res             1
stemp            res             1
sBits            res             1
sindex           res             1
soutData         res             1

                 FIT 496

DAT 'ADC_PASM
MAX11040K         org         0
          mov     adcSemPtr,     par
          mov     elementsPtr,   par                    ' where are we in the buffer?
          mov     bufferBasePtr, par                    ' where is the start of the buffer? 
          add     elementsPtr,   #4
          add     bufferBasePtr, #8

          rdlong  adcSem,        adcSemPtr            ' get a copy of the semaphore address

          call    #SETUP_PINS                           ' lower CS line

':adcLoop
'          xor     outa,          testMask
'          jmp     #:adcLoop
          
          call    #SETUP_ADC_REGS

' take care of a bit of book-keeping; (re)initializing things
:bufferLoop
          mov     blockCount,    #BLOCKS_IN_BUFFER       ' keep track of how many blocks are full; init here
          mov     bufferPtr,     bufferBasePtr           ' get copy of the start of the buffer


:waitForSpace     ' wait here until there is at least one free space in queue
          rdlong  atemp,         elementsPtr
          cmp     atemp,         #BLOCKS_IN_BUFFER wz   ' if temp==BLOCKS_IN_BUFFER set Z=1
   if_z   jmp     #:waitForSpace                        ' finished<>0 ->Z=0;   move on


' if we made it here, there must be room in the queue so let's fill a block (512 bytes)

' first 2 things into the 512 byte buffer are data1 and data2
' followed by 
          mov     slotsInBlock,  #SLOTS_IN_BLOCK        ' number of time slices in each block
          wrlong  data1,         bufferPtr
          add     bufferPtr,     #4
          wrlong  data2,         bufferPtr
          add     bufferPtr,     #4

:blockLoop
          waitpeq azero,         drdyMask               ' wait for DRDY to go LOW
          cmp     slotsInBlock,  #SLOTS_IN_BLOCK   wz   ' is this the first sample?
   if_z   mov     cntsAtStart,   cnt                    ' get cnts at start          

          call    #LOWER_CS                             ' lower CS line

          ' write RDR
          mov     data,         #RDR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          ' read Z connected to RED/BROWN phone
          call    #READ_SAMPLE  ' sample is now in data 
          'mov     data,         #$A
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' read N/S connected to ORANGE/YELLOW
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$B
          call    #SWAP_AND_WRITE_LONG_TO_HUB
                                
          ' read E/W connected to BLUE/GREEN
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$C
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' read D                                      ' we need to read this 4 sample even
          call    #READ_SAMPLE  ' sample is now in data
          'mov     data,         #$D
          mov     data,         cnt
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          ' raise CS
          call    #RAISE_CS                             ' raise CS line
          
          djnz    slotsInBlock, #:blockLoop

' now we've finished one complete block.  Get the meta-data associated with this block
          mov     cntsAtEnd,    cnt
          
' wait here until we can lock the share GPS memory
:WaitForGPS
          lockset gpsSem        wc
'          xor     outa,         testMask
   if_c   jmp     #:WaitForGPS               ' if adcSem = 0 loop 

          mov     atemp,        timeDatePtr

          rdlong  data,         atemp        ' get ymd from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          rdlong  data,         atemp        ' get hms from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          rdlong  data,         atemp        ' get cnts@PPS from main cog
          add     atemp,        #4
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          lockclr gpsSem                     ' clear gpsSem for others to use

          mov     data,         cntsAtStart
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          mov     data,         cntsAtEnd
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          mov     data,         cksum          
          call    #SWAP_AND_WRITE_LONG_TO_HUB

          
' we've filled one block so lets update shared variable blocksInFIFO
          call    #INCREMENT_ELEMENTS

' now jump back up to see if there's any free space for another block of data
          djnz    blockCount,   #:waitForSpace

' once we've filled all blocks in buffer, we need to point things back to the top of buffer
          jmp     #:bufferLoop

:stop     waitpeq sclkMask,sclkMask
          jmp     #:stop          

{******************************** INCREMENT_ELEMENTS **********************************************
Function that gets the shared semaphore, increments the shared variable:
elementsPtr
then releases the semaphore
***************************************************************************************************}
INCREMENT_ELEMENTS

          cmp     azero,         #0           wz         ' set Z=1 
'          muxz    outa,          testMask
          
:WaitForSem
          lockset adcSem        wc
   if_c   jmp     #:WaitForSem                          ' if adcSem = 0 loop 

          rdlong  atemp,        elementsPtr             ' copy number of blocks in buffer so we can add to it
          add     atemp,        #1                      ' increment count
          wrlong  atemp,        elementsPtr             ' write the new value back to HUB
          lockclr adcSem                                ' clear adcSem for others to use
'          xor     outa,          testMask

INCREMENT_ELEMENTS_ret      ret

{******************************** WRITE_LONG_TO_HUB *******************************************************
function that will swap the endianness of the long and write it to hub then increment the
hub pointer
***************************************************************************************************}
WRITE_LONG_TO_HUB
                                                ' soutData = b3 b2 b1 b0
          wrlong  data,         bufferPtr    ' copy it back to hub in new location
          add     bufferPtr,    #4

WRITE_LONG_TO_HUB_ret    ret
{******************************** SWAP_AND_WRITE_LONG_TO_HUB *******************************************************
function that will swap the endianness of the long and write it to hub then increment the
hub pointer
***************************************************************************************************}
SWAP_AND_WRITE_LONG_TO_HUB
                                                ' soutData = b3 b2 b1 b0
          mov     swapTemp,     endianMask      '       t1 = 00 FF 00 FF
          ror     data,         #8              ' soutData = b0 b3 b2 b1
          and     swapTemp,     data        '       t1 = 00 b3 00 b1
          xor     data,         swapTemp           ' soutData = b0 00 b2 00
          rol     swapTemp,     #16             '       t1 = 00 b1 00 b3
          or      data,         swapTemp           ' soutData = b0 b1 b2 b3 

          wrlong  data,         bufferPtr    ' copy it back to hub in new location
          add     bufferPtr,    #4

SWAP_AND_WRITE_LONG_TO_HUB_ret    ret
{******************************** SETUP_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_PINS
' Setup CS pin: Preset high; set to output
          mov     atemp,         #1          wz         ' Z=0; 
          muxnz   outa,          csMask                 ' Preset OUTA to high        "1"
          muxnz   dira,          csMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup SCLK pin: Preset high; set to output
          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxnz   outa,          sclkMask               ' Preset OUTA to high        "1"
          muxnz   dira,          sclkMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup MOSI pin: Preset low; set to output
          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          mosiMask               ' Preset OUTA to low         "0"
          muxnz   dira,          mosiMask               ' Set    DIRA to HIGH=OUTPUT "1"

'' Setup TESTPT pin: Preset low; set to output
'          mov     atemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          testMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          testMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_PINS_ret    ret


{******************************** SETUP_ADC_REGS *******************************************************
Short function to setup the ADC register values- sample rate, etc
CS is HIGH on entry
CS is HIGH on exit
***************************************************************************************************}
SETUP_ADC_REGS
' reset ADC
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WCR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         #%0_1_0_0_0_0_00        ' reset to be written to control register     
          call    #WRITE_BYTE                           ' Send it
          call    #RAISE_CS                             ' raise CS line
          
' set ADC to 24-bit data
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WCR                    ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         #%0_0_1_0_0_0_00        ' set 24-bit enable     
          call    #WRITE_BYTE                           ' Send it
          call    #RAISE_CS                             ' raise CS line
  
' setup sample rate
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #WDRCR                  ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          mov     data,         wdrcrVal                ' send data rate     
          call    #WRITE_WORD                           ' Send it
          call    #RAISE_CS                             ' raise CS line

' read sample rate
{ removed the following because we've gotten past this and there's no
  easy way to put this value into HUB at the moment
          call    #LOWER_CS                             ' lower CS line
          mov     data,         #RDRCR                  ' put the address into outLong
          call    #WRITE_BYTE                           ' Send it
          
          call    #READ_WORD                            ' Send it
          call    #RAISE_CS                             ' raise CS line
          wrlong  data,         responsePtr
}
SETUP_ADC_REGS_ret    ret

{******************************** WRITE_BYTE *******************************************************
This command will write the LOWEST 8 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_BYTE
          mov     bits1,        #8         wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          rol     data,         #24             ' rotate data left so byte of interest is in upper 8 bits

:wbyteLoop
          muxnz   outa,         sclkMask        ' RAise clock Pin
          rol     data,         #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mosiMask        ' Set OUTA[mosiMask]=C
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:wbyteLoop     ' Decrement loop count.


WRITE_BYTE_ret    ret


{******************************** WRITE_WORD *******************************************************
This command will write the LOWEST 16 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_WORD
          mov     bits1,        #16        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          rol     data,         #16             ' rotate data left so byte of interest is in upper 8 bits

:wwordLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          rol     data,         #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mosiMask        ' Set OUTA[mosiMask]=C
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:wwordLoop     ' Decrement loop count.

WRITE_WORD_ret    ret


{******************************** READ_WORD *******************************************************
This command will read 16 BITS of data on DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
READ_WORD
          mov     bits1,        #16        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          mov     data,         azero           ' clear out input register     

:rwordLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          test    misoMask,     ina        wc   ' C=INA[misoMask]
          rcl     data,         #1              ' shift C left into data
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:rwordLoop     ' Decrement loop count.

READ_WORD_ret    ret

{******************************** READ_SAMPLE*******************************************************
This command will read 24 BITS of data on DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
READ_SAMPLE
          mov     bits1,        #24        wz   ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          mov     data,         azero           ' clear out input register     

:rSampleLoop
          muxnz   outa,         sclkMask        ' Rause clock Pin
          test    misoMask,     ina        wc   ' C=INA[misoMask]
          rcl     data,         #1              ' shift C left into data
          xor     outa,         sclkMask        ' lower the clock line
          djnz    bits1,        #:rSampleLoop   ' Decrement loop count.

          ' no longer need to shift left then right.  Our samples are 24 bits and we are using the
          ' upper 8 bits for channel number and gain
          shl     data,         #8              ' shift left so bit 23 moves into bit 31
          sar     data,         #8              ' shift back right while preserving the sign
READ_SAMPLE_ret    ret

{******************************** WRITE_DATA_TO_HUB *******************************************************
This command writes the recently collected data and advances the pointers.
This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_DATA_TO_HUB
          wrlong  data,         bufferPtr               ' write A0 sample to buffer
          mov     data,         cnt                     ' grab current cnt
          add     bufferPtr,    #4                      ' increment bufferPtr
          sub     data,         startCnts               ' data1=data1-startCnts
          wrlong  data,         bufferPtr               ' write current
          add     bufferPtr,    #4                      ' increment bufferPtr

WRITE_DATA_TO_HUB_ret    ret

{******************************** WRITE_CNT_TO_HUB *******************************************************
This command writes the recently collected data and advances the pointers.
This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
WRITE_CNT_TO_HUB
          wrlong  data,         bufferPtr               ' write A0 sample to buffer
          mov     data,         cnt                     ' grab current cnt
          add     bufferPtr,    #4                      ' increment bufferPtr
          sub     data,         startCnts               ' data1=data1-startCnts
          wrlong  data,         bufferPtr               ' write current
          add     bufferPtr,    #4                      ' increment bufferPtr

WRITE_CNT_TO_HUB_ret    ret

{******************************** LOWER_CS *********************************************************
This command simply lowers the CS line.  Nothing else.
***************************************************************************************************}
LOWER_CS
          cmp     azero,        azero       wz      ' set Z=1
          muxnz   outa,         csMask              ' set OUTA[csMask]=0
LOWER_CS_ret      ret

{******************************** RAISE_CS *********************************************************
This command simply raises the the SCLK and CS line.  Nothing else.
***************************************************************************************************}
RAISE_CS
          cmp     azero,        azero       wz  ' set Z=1
          muxz    outa,         sclkMask        ' set OUTA[clkMask]=1
          muxz    outa,         csMask          ' set OUTA[csMask]=1
RAISE_CS_ret      ret



{******************************** WAIT_FOR_PPS*****************************************************
A couple of lines used to wait for a rising PPS.
***************************************************************************************************}
WAIT_FOR_PPS
          waitpne   ppsMask, ppsMask         ' wait for ppsMask to go LOW; fall through if ppsMask is low
          waitpeq   ppsMask, ppsMask         ' wait for ppsMask to go HIGH

WAIT_FOR_PPS_ret  ret
{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

csMask      long            |< ADC_CS               ' ADC CS line
drdyMask    long            |< ADC_DRDYOUT           
sclkMask    long            |< ADC_SCLK
misoMask    long            |< ADC_MISO
mosiMask    long            |< ADC_MOSI
ppsMask     long            |< GPS_PPS
'testMask    long            |< ADC_TEST

' Variables being passed in
samples          long            0
wdrcrVal         long            0
rdrcrVal         long            0
wsicrVal         long            0
rsicrVal         long            0
wcrVal           long            0
rcrVal           long            0
fsampc           long            0
fsampf           long            0
three            long            3
two              long            2
one              long            1
azero            long            0
codeA            long            0
codeB            long            0
codeC            long            0
codeD            long            0
blockCount       long            0
ymdAtEnd         long            0    ' ymd at end
hmsAtEnd         long            0    ' hms at end
cntsAtPPS2       long            0    ' cnts @ PPS
cntsAtStart      long            0    ' cnts @ start
cntsAtEnd        long            0    ' cnts @ end
cksum            long            0    ' cksumB | cksumA
timeDatePtr      long            0
gpsSem           long            0

data1            long            0    ' seismic/gps | sample rate | gainA | gainB | gainC | gainD
data2            long            0    ' version number | samples in this packet | clock lock status
endianMask       long            $00FF_00FF         ' used for byte swap

swapTemp         res             1
atemp            res             1
adcSemPtr        res             1
adcSem           res             1
elementsPtr      res             1
bufferPtr        res             1
bufferBasePtr    res             1
slotsInBlock     res             1
startCnts        res             1
curCnts          res             1
data             res             1
dataA            res             1
dataB            res             1
dataC            res             1
dataD            res             1
bits1            res             1
a0               res             1
a1               res             1
a2               res             1
a3               res             1
                 FIT 496

DAT 'SPI_MASTER                 
SPI_MASTER        org            0

          call    #SETUP_MASTER_PINS                           ' lower CS line
':toggle
'          xor     outa,          mtestMask
'          jmp     #:toggle


' take care of a bit of book-keeping; (re)initializing things
' set pointer to beginning of outBuffer, reset 
:mblockLoop
          mov     mindex,         mBytesInBlock        ' keep track of how many blocks are full; init here



          waitpeq mzero,         mirqMask                 ' wait for IRQ to go LOW
          call    #M_LOWER_CS                               ' lower CS line

          nop
          nop
          nop

:masterLoop
          ' get next long to shift out
          call    #M_WRITE_BYTE

          djnz    mindex,        #:masterLoop

          call    #M_RAISE_CS                             ' raise CS line


          jmp     #:mblockLoop          

{******************************** WRITE_WORD *******************************************************
This command will write the LOWEST 16 BITS of data MSB to the DATA line.  ADC latches data on falling
edge.  This routine does not touch CS at all.   SCLK is low on exit
***************************************************************************************************}
M_WRITE_BYTE
          mov     moutData,     #$99'mindex
          shl     moutData,     #24             ' shift things over to MSBit is in bit 31
          mov     mBits,        #8        wz    ' setup number of bits in write; set Z=0 
                                                ' Z=0; Z flag is set (1) if Value equals zero
          nop
          nop
          nop
:wByteLoop
          rol     moutData,     #1         wc   ' shift data left 1; C=bit shifted out
          muxc    outa,         mmosiMask       ' Set OUTA[mosiMask]=C
          muxnz   outa,         msclkMask       ' Rause clock Pin
'          test    mmisoMask,    ina        wc   ' C=INA[misoMask]
'          rcl     minData,      #1              ' shift C left into data
          nop
          nop
          xor     outa,         msclkMask       ' lower the clock line

          djnz    mBits,        #:wByteLoop     ' Decrement loop count.
          muxz    outa,         mmosiMask       ' make sure MOSI is low on exit
          muxz    outa,         msclkMask       ' make sure MOSI is low on exit

M_WRITE_BYTE_ret    ret


{******************************** SETUP_PINS *******************************************************
Short function to setup the SPI and test pins.  Only used once and it's really
for cleaning up the code more than anything.
***************************************************************************************************}
SETUP_MASTER_PINS
' Setup CS pin: Preset high; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxnz   outa,          mcsMask                 ' Preset OUTA to high        "1"
          muxnz   dira,          mcsMask                 ' Set    DIRA to HIGH=OUTPUT "1"

' Setup SCLK pin: Preset high; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          msclkMask               ' Preset OUTA to low         "0"
          muxnz   dira,          msclkMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup MOSI pin: Preset low; set to output
          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
          muxz    outa,          mmosiMask               ' Preset OUTA to low         "0"
          muxnz   dira,          mmosiMask               ' Set    DIRA to HIGH=OUTPUT "1"

' Setup TESTPT pin: Preset low; set to output
'          mov     mtemp,         #1          wz         ' Z=0; put $01 into temp; Z=1 if #1=0
'          muxz    outa,          mtestMask               ' Preset OUTA to low         "0"
'          muxnz   dira,          mtestMask               ' Set    DIRA to HIGH=OUTPUT "1"

SETUP_MASTER_PINS_ret    ret

{******************************** LOWER_CS *********************************************************
This command simply lowers the CS line.  Nothing else.
***************************************************************************************************}
M_LOWER_CS
          cmp     mzero,        mzero        wz      ' set Z=1
          muxnz   outa,         mcsMask              ' set OUTA[csMask]=0
M_LOWER_CS_ret      ret

{******************************** RAISE_CS *********************************************************
This command simply raises the the SCLK and CS line.  Nothing else.
***************************************************************************************************}
M_RAISE_CS
          cmp     mzero,        mzero        wz  ' set Z=1
          muxz    outa,         mcsMask          ' set OUTA[csMask]=1
M_RAISE_CS_ret      ret

                                     
{**************************************************************************************************
' Pin Masks
***************************************************************************************************}

mcsMask      long            |< SLAVE_CS               '
msclkMask    long            |< SLAVE_CLK
mmisoMask    long            |< SLAVE_SOMI
mmosiMask    long            |< SLAVE_SIMO
mirqMask     long            |< SLAVE_IRQ
mzero        long            0
mBytesInBlock     long       BYTES_IN_BLOCK

mtemp             res             1
mBits             res             1
mindex            res             1
masterPtr         res             1
masterBasePtr     res             1
moutData          res             1
minData           res             1
                 FIT 496    
                 