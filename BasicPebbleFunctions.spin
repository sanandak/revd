{BasicPebbleFunctions - pgb}
' File that houses many of the basic pebble functions
' this should make editing things simpler.  This file can
' be open at the same time as the main file.


CON ' I2C expander constants
  EXPANDER_1    = %0111_111_0   ' bits 7-4 are taken from Datasheet; bits 4-1 are set by layout
  EXPANDER_2    = %0111_110_0   ' bits 7-4 are taken from Datasheet; bits 4-1 are set by layout

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
  PGA_A_CS    = BIT_4
  PGA_B_CS    = BIT_5
  PGA_C_CS    = BIT_6
  PGA_D_CS    = BIT_7

  LED1        = BIT_0
  LED2        = BIT_1
  EN_5V       = BIT_2
  GUMSTIX_EN  = BIT_3
  UBLOX_EN    = BIT_4
  OLED_EN     = BIT_5
  MAG_ACC_EN  = BIT_6
  GPS_RESET   = BIT_7        ' this is active LOW

CON ' MUX settings
  INTERNAL = 0
  EXTERNAL = 1
  
CON ' Pin map
  EXT_OSC       =  0
  GPS_PPS       =  1
  GPS_RX_FROM   =  2
  GPS_TX_TO     =  3

  SPARE         =  4 ' use spare pin as test point

  ADC_DRDYOUT   =  5
  ADC_MISO      =  6 ' connected to DOUT on MAX11040K
  ADC_MOSI      =  7 ' connected to DIN  on MAX11040K
  ADC_SCLK      =  8
  ADC_CS        =  9

  SRAM_CS       = 10
  SRAM_MOSI     = 11
  SRAM_MISO     = 12
  SRAM_CLK      = 13

  WAKEUP        = 14
  REED_SWITCH   = 15

  SLAVE_SCLK    = 16 
  SLAVE_SIMO    = 17 
  SLAVE_SOMI    = 18 
  SLAVE_CS      = 19 
  SLAVE_IRQ     = 20 

  SHARED_CLK    = 21
  SHARED_MOSI   = 22
  SHARED_MISO   = 23
  OLED_CS       = 24
  DAC_CS        = 25
  RADIO_CS      = 26

  ONE_WIRE      = 27  

  SCL           = 28
  SDA           = 29
  
  DEBUG_TX_TO   = 30
  DEBUG_RX_FROM = 31

CON ' EEPROM constants
  EEPROM_ADDR   = %1010_0000

CON ' RTC and SRAM addresses and constants
  RTC_ADDR       = %1101_1110
  EUI_ADDR       = %1010_1110

  TIME_DATA_BASE = $00
  RTCSEC         = $00
  RTCMIN         = $01
  RTCHOUR        = $02
  RTCWKDAY       = $03
  RTCDATE        = $04
  RTCMTH         = $05
  RTCYEAR        = $06

  VBATEN         = BIT_3
  ST             = BIT_7
  CONTROL        = $07
  OSCTRIM        = $08

  RTC_CFG_BASE   = $07
  RTC_SRAM_BASE  = $20          ' starting address of the SRAM in the RTC
  EUI_BASE       = $F2          ' See section 6.4 of the datasheet
  
  
CON  'True/False constants
  MY_TRUE   = 0
  MY_FALSE  = 1

CON ' REED switch button states
  NOT_PRESSED       = 1         ' Reed switch is pulled high normally 
  PRESSED           = 0         ' Pressed is actually logic 0  
  
CON ' Acq enumerations
 #0, TRIGGERED, CONTINUOUS, STAY_ASLEEP
 
CON ' DAC constants
  WRITE_AND_UPDATE = $0030_0000  ' code for DAC that performs write and update

CON ' OLED CONSTANTS
  CLEAR_DISPLAY   = %00_0000_0001
  RETURN_HOME     = %00_0000_0010
  ENTRY_MODE_0    = %00_0000_0100  ' auto-DEincrement no shift
  ENTRY_MODE_1    = %00_0000_0101  ' auto-DEcrement   shift display RIGHT
  ENTRY_MODE_2    = %00_0000_0110  ' auto-INcrement   no shift
  ENTRY_MODE_3    = %00_0000_0111  ' auto-INcrememnt  shift display LEFT
  DISPLAY_ON      = %00_0000_1101  ' display ON   cursor OFF  blinking OFF
  DISPLAY_OFF     = %00_0000_1000  ' display OFF
  FUNCTION_SET    = %00_0011_1000  ' set to english
  CURSOR_SHIFT_0  = %00_0001_0000  ' shift CURSOR to the left
  CURSOR_SHIFT_1  = %00_0001_0100  ' shift CURSOR to the right
  CURSOR_SHIFT_2  = %00_0001_1000  ' shift DISPLAY to the left
  CURSOR_SHIFT_3  = %00_0001_1100  ' shift DISPLAY to the right
  SET_CGRAM_ADD   = %00_0100_0000  ' lower 6 bits of this command should be the address
  SET_DDRAM_ADD   = %00_1000_0000  ' lower 7 bits of this command should be the address
  DEGREES         = %1101_1111
  TICK            = %0010_0111
  BLOCK           = %1111_1111     ' all pixel high
  RS_BIT          = %10_0000_0000
  RW_BIT          = %01_0000_0000
  A0_B0_C0        = %000
  A0_B0_C1        = %001
  A0_B1_C0        = %010
  A0_B1_C1        = %011
  A1_B0_C0        = %100
  A1_B0_C1        = %101
  A1_B1_C0        = %110
  A1_B1_C1        = %111

CON ' LSM Device Addresses
'' LSM303DLHC device addresses
' For linear acceleration the default (factory) 7-bit slave address is 0011001xb.
' For magnetic sensors the default (factory) 7-bit slave address is 0011110xb.
  ACC_ADDR = %001_1001 '   
  MAG_ADDR = %001_1110 ' 
  
CON ' LSM register constants
'' register addresses for LSM303DLHC
  LSM303_CTRL_REG1_A       = $20
'  LSM303_CTRL_REG2_A       = $21
'  LSM303_CTRL_REG3_A       = $22
  LSM303_CTRL_REG4_A       = $23
'  LSM303_CTRL_REG5_A       = $24
'  LSM303_CTRL_REG6_A       = $25 ' DLHC only
'  LSM303_HP_FILTER_RESET_A = $25 ' DLH, DLM only
'  LSM303_REFERENCE_A       = $26
'  LSM303_STATUS_REG_A      = $27

  LSM303_OUT_X_L_A         = $28
  LSM303_OUT_X_H_A         = $29
  LSM303_OUT_Y_L_A         = $2A
  LSM303_OUT_Y_H_A         = $2B
  LSM303_OUT_Z_L_A         = $2C
  LSM303_OUT_Z_H_A         = $2D

'  LSM303_FIFO_CTRL_REG_A   = $2E ' DLHC only
'  LSM303_FIFO_SRC_REG_A    = $2F ' DLHC only

'  LSM303_INT1_CFG_A        = $30
'  LSM303_INT1_SRC_A        = $31
'  LSM303_INT1_THS_A        = $32
'  LSM303_INT1_DURATION_A   = $33
'  LSM303_INT2_CFG_A        = $34
'  LSM303_INT2_SRC_A        = $35
'  LSM303_INT2_THS_A        = $36
'  LSM303_INT2_DURATION_A   = $37

'  LSM303_CLICK_CFG_A       = $38 ' DLHC only
'  LSM303_CLICK_SRC_A       = $39 ' DLHC only
'  LSM303_CLICK_THS_A       = $3A ' DLHC only
'  LSM303_TIME_LIMIT_A      = $3B ' DLHC only
'  LSM303_TIME_LATENCY_A    = $3C ' DLHC only
'  LSM303_TIME_WINDOW_A     = $3D ' DLHC only

  LSM303_CRA_REG_M         = $00
  LSM303_CRB_REG_M         = $01
  LSM303_MR_REG_M          = $02

  LSM303_OUT_X_H_M         = $03
  LSM303_OUT_X_L_M         = $04
  LSM303_OUT_Z_H_M         = $05 
  LSM303_OUT_Z_L_M         = $06
  LSM303_OUT_Y_H_M         = $07
  LSM303_OUT_Y_L_M         = $08

'  LSM303_SR_REG_M          = $09
'  LSM303_IRA_REG_M         = $0A
'  LSM303_IRB_REG_M         = $0B
'  LSM303_IRC_REG_M         = $0C

'  LSM303_WHO_AM_I_M        = $0F ' DLM only

  LSM303_TEMP_OUT_H_M      = $31 ' DLHC only
  LSM303_TEMP_OUT_L_M      = $32 ' DLHC only

  LSM303DLH_OUT_Y_H_M      = $05
  LSM303DLH_OUT_Y_L_M      = $06
  LSM303DLH_OUT_Z_H_M      = $07
  LSM303DLH_OUT_Z_L_M      = $08

  LSM303DLM_OUT_Z_H_M      = $05
  LSM303DLM_OUT_Z_L_M      = $06
  LSM303DLM_OUT_Y_H_M      = $07
  LSM303DLM_OUT_Y_L_M      = $08

  LSM303DLHC_OUT_Z_H_M     = $05
  LSM303DLHC_OUT_Z_L_M     = $06
  LSM303DLHC_OUT_Y_H_M     = $07
  LSM303DLHC_OUT_Y_L_M     = $08

CON ' flags for ACC CTRL_REG1_A                       ORD1 | XEN | YEN | ZEN
  ORD3 = BIT_7
  ORD2 = BIT_6
  ORD1 = BIT_5
  ORD0 = BIT_4
  
  LPEN = BIT_3
  ZEN  = BIT_2
  YEN  = BIT_1
  XEN  = BIT_0

CON ' flags for ACC CTRL_REG4_A
  BLU = BIT_7 ' block data output. 0=continutous update; 1 block until data have been read
  BLE = BIT_6 ' Big/litte endian. 0=data LSB @ lower address; 1=data MSB @ lower address
  FS2 = %00  ' +-2g
  FS4 = %01 
  FS8 = %10 
  FS16 = %11 
  HR = BIT_3

'mag settings
' flags for CRA reg
  TEMP_EN = BIT_7
  DO_75   = %000 << 2' 0.75
  DO1_5   = %001 << 2 ' 1.5...
  DO3_0   = %010 << 2
  DO7_5   = %011 << 2
  DO15_0  = %100 << 2 '15
  DO30_0  = %101 << 2
  DO75_0  = %110 << 2
  DO220_0 = %111 << 2
' flags for CRB reg
  GN1_3 = %001 << 5 ' +-1.3gauss
  GN1_9 = %010 << 5
  GN2_5 = %011 << 5
  GN4_0 = %100 << 5
  GN4_7 = %101 << 5
  GN5_6 = %110 << 5
  GN8_1 = %111 << 5

' flags for MR REG
  MD_CC = %00 ' continuous conversion
  MD_ONE = %01 ' once
  MD_SLEEP = %11

CON ' ACK/NAK
  ACK = 0
  NAK = 1
  I2C_READ  = 1
  I2C_WRITE = 0
  
OBJ
  I2C:       "Pebble_I2C_Object"

VAR
  byte expVal1, expVal2
  byte eui_48[8]      ' storage for the 8-bytes read from the RTC/Memory
  byte rtcSram[64]   ' 64-byte SRAM storage
  byte mag[6]
  byte rtcTime[7]
PUB INIT : response
'  DIRA~ ' set everything to input
' setup the I2C bus pins
  I2C.INIT(SDA, SCL)

' SRAM is powered but not used; hold it in reset with CS high
  DIRA[SRAM_CS]     := 1           ' hold SRAM in RESET
  OUTA[SRAM_CS]     := 1           ' hold SRAM in RESET
  DIRA[SRAM_MOSI]   := 0
  DIRA[SRAM_MISO]   := 0
  DIRA[SRAM_CLK]    := 0



PUB SET_EXPANDER_TO_LOW_POWER : response 
' put the 2 expanders (and the LEDs they contain) into a know and low-power state
' EXP_1 : PGA_D_CS | PGA_C_CS | PGA_B_CS | PGA_A_CS | MUX2SEL_3-4 | MUX2SEL_1-2 | MUX1SEL_3-4 | MUX1SEL_1-2
  expVal1 := $00' TURN EVERYTHING OFF PGA_D_CS | PGA_C_CS | PGA_B_CS | PGA_A_CS | %0000
  EXPANDER_WRITE(EXPANDER_1, expVal1)

' EXP_2 : GPS_RESET | MAG_ACC_EN | OLED_EN | UBLOX_EN | GUMSTIX_EN | 5V_ENABLE | LED1003 | LED1002     
  expVal2 := %00000111  ' turn components OFF and turn LEDS OFF (high)
  EXPANDER_WRITE(EXPANDER_2, expVal2)

'  MAG_ACC_OFF
'  GPS_OFF
'  OLED_OFF
'  GUMSTIX_OFF

  ' now let's lower shutdown the ADC by setting shutdown bit (7) high
  response := SHUTDOWN_ADC

  DIRA[ADC_DRDYOUT] := 0
  DIRA[ADC_MISO]    := 0
  DIRA[ADC_MOSI]    := 0
  DIRA[ADC_SCLK]    := 0
  DIRA[ADC_CS]      := 0
  
  ANALOG_OFF

' gumstix is off so pull set spare pin to input
  DIRA[SPARE]       := 1
  OUTA[SPARE]       := 1        ' pull this high

' SRAM is powered but not used; hold it in reset with CS high
  DIRA[SRAM_CS]     := 1        ' hold SRAM in RESET
  OUTA[SRAM_CS]     := 1        ' hold SRAM in RESET
  DIRA[SRAM_MOSI]   := 0
  DIRA[SRAM_MISO]   := 0
  DIRA[SRAM_CLK]    := 0

' controller bus isn't used when sleeping; make all these lines inputs
  DIRA[SLAVE_SCLK]   := 0
  DIRA[SLAVE_SIMO]  := 0
  DIRA[SLAVE_SOMI]  := 0
  DIRA[SLAVE_CS]    := 0
  DIRA[SLAVE_IRQ]   := 0

' shared lines are not used when sleeping; set them to inputs  
  DIRA[SHARED_CLK]  := 0        ' set as input
  DIRA[SHARED_MOSI] := 0        ' set as input
  DIRA[SHARED_MISO] := 0        ' set as input

' oled is not powered so make OLED_CS an input
  DIRA[OLED_CS]     := 0        ' set as input

' DAC is still powered so make sure we hold this line high
  DIRA[DAC_CS]      := 1        ' set as out
  OUTA[DAC_CS]      := 1        ' hold line high

' RADIO is still powered so make sure we hold this line high
  DIRA[RADIO_CS]    := 1        ' set as out
  OUTA[RADIO_CS]    := 1        ' hold line high

' ONE_WIRE temp sensor is not being used at the moment so pull it up to avoid burning current through R1102
  DIRA[ONE_WIRE]    := 1  
  OUTA[ONE_WIRE]    := 1  

' I2C lines should be pulled high in this cog
  DIRA[SCL]         := 1
  OUTA[SCL]         := 1
  DIRA[SDA]         := 1
  OUTA[SDA]         := 1
                                          

PUB LEDS_ON
' turn the LEDS on the I2C expander ON by setting the bits LOW
  expVal2 &= !(LED2 | LED1)'%1111_1100
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2
  
PUB LEDS_OFF
' toggle the LEDS
  expVal2 |= (LED2 | LED1)'%0000_0011
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2

PUB LED1_ON
' turn LED1 on the I2C expander ON by setting the bits LOW
  expVal2 &= !LED1 '%1111_1110
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2
  
PUB LED1_OFF
' turn LED1 OFF
  expVal2 |= LED1
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2

PUB LED2_ON
' turn LED2 on the I2C expander ON by setting the bits LOW
  expVal2 &= !LED2 '%1111_1101
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2
  
PUB LED2_OFF
' turn LED2 OFF
  expVal2 |= LED2
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  return expVal2

PUB ANALOG_ON
' pull all the PGA CS lines high
' set MUXes to internal phones
  expVal1 |= PGA_D_CS | PGA_C_CS | PGA_B_CS | PGA_A_CS ' raise all the PGA_CS lines; set all MUX values to low meaning internal geophones  
  expVal2 |= EN_5V   ' raise the 5V line

  EXPANDER_WRITE(EXPANDER_1, expVal1)
  EXPANDER_WRITE(EXPANDER_2, expVal2)
'  WRITE_TO_OLED(0, 1, string("Analog ON       "))

PUB ANALOG_OFF
' pull all the PGA CS lines high
' set MUXes set to internal phones
  expVal1 := PGA_D_CS | PGA_C_CS | PGA_B_CS | PGA_A_CS ' raise all the PGA_CS lines; set all MUX values to low meaning internal geophones  
  expVal2 &= !EN_5V   ' raise the 5V line

  EXPANDER_WRITE(EXPANDER_1, expVal1)
  EXPANDER_WRITE(EXPANDER_2, expVal2)
'  WRITE_TO_OLED(0, 1, string("Analog OFF      "))
  
PUB GUMSTIX_ON
' Turn on the switch that sends allows 3.3V power to pass to the Gumstix
  expVal2 |= GUMSTIX_EN 
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  PAUSE_MS(2000)                ' give the gumstix time to boot                                 
   
PUB GUMSTIX_OFF
' Turn off the switch that sends allows 3.3V power to pass to the Gumstix
  expVal2 &= !GUMSTIX_EN 
  EXPANDER_WRITE(EXPANDER_2, expVal2)
 
PUB GPS_ON
' Turn on the switch that allows 3.3V power to pass to the UBLOX
  expVal2 |= (UBLOX_EN)  
  EXPANDER_WRITE(EXPANDER_2, expVal2)

' Now pull reset line high, wait, then low, wait, pull high again
  expVal2 |= (GPS_RESET)  
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  PAUSE_MS(150)
  expVal2 &= !GPS_RESET  
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  PAUSE_MS(150)
  expVal2 |= (GPS_RESET)  
  EXPANDER_WRITE(EXPANDER_2, expVal2)

PUB GPS_OFF
' Turn off the switch that allows 3.3V power to pass to the UBLOX
  expVal2 &= !(UBLOX_EN | GPS_RESET)  ' lower both lines
  EXPANDER_WRITE(EXPANDER_2, expVal2)

PUB MAG_ACC_ON
' Turn on the switch that allows 3.3V power to pass to the MAG/ACC
  expVal2 |= MAG_ACC_EN  ' and turn on both MAG/ACC
  EXPANDER_WRITE(EXPANDER_2, expVal2)

PUB MAG_ACC_OFF
' Turn off the switch that allows 3.3V power to pass to the MAG/ACC
  expVal2 &= !MAG_ACC_EN  ' and turn on both MAG/ACC
  EXPANDER_WRITE(EXPANDER_2, expVal2)

PUB OLED_OFF
' Turn off the switch that allows 3.3V power to pass to the OLED
  expVal2 &= !(OLED_EN)  ' and turn off OLED
  EXPANDER_WRITE(EXPANDER_2, expVal2)
  OUTA[OLED_CS] := 0   ' lower this so we aren't powering the oled from the prop
  DIRA[OLED_CS] := 0   ' make this an input
  
PUB OLED_ON
' setup the shared SPI pins
  DIRA[SHARED_MOSI] := 1
  OUTA[SHARED_MOSI] := 0

  DIRA[SHARED_MISO] := 0
  
  DIRA[SHARED_CLK] := 1
  OUTA[SHARED_CLK] := 0       

  ' setup the OLED_CS pin as an output and high
  DIRA[OLED_CS]  := 1
  OUTA[OLED_CS]  := 1
  
' Turn on the switch that allows 3.3V power to pass to the OLED
  expVal2 |= OLED_EN  ' and turn on  OLED
  EXPANDER_WRITE(EXPANDER_2, expVal2)

  PAUSE_MS(1000)
  OLED_INIT

PUB OLED_INIT
' method that sets up the display for use
  OLED_COMMAND($38)   ' Function Set
  OLED_COMMAND($08)   ' Display OFF
  OLED_COMMAND($01)   ' Display Clear
  PAUSE_MS(1)         ' Additional delay to allow Display Clear to complete
  OLED_COMMAND($06)   ' Entry Mode Set
  OLED_COMMAND($02)   ' Home Command

  ' now create the four custom characters
  INIT_A0_B0_C0
  INIT_A0_B0_C1
  INIT_A0_B1_C0
  INIT_A0_B1_C1
  INIT_A1_B0_C0
  INIT_A1_B0_C1
  INIT_A1_B1_C0
  INIT_A1_B1_C1

  OLED_COMMAND($0C)              ' Turn ON OLED

PUB OLED_COMMAND(Command)
'// Command Write
'// RS = 0 and RW = 0.
  OUTA[OLED_CS]     := 0   ' lower CS line to enable the display
  SHIFT_OUT_TO_OLED(Command)     ' Call the data byte driver function
  OUTA[OLED_CS]     := 1   ' raise CS line to disable the display
  PAUSE_MS(1)              ' Provide time for command to execute

PUB OLED_DATA(RAM_data)
'// RAM Write
'// RS = 1 and RW = 0.
  OUTA[OLED_CS]     := 0   ' lower CS line to enable the display
  RAM_data |= $0200        ' Enable the RS bit
  SHIFT_OUT_TO_OLED(RAM_data)    ' Call the data byte driver function
  OUTA[OLED_CS]     := 1   ' raise CS line to disable the display
  'PAUSE_MS(1)              ' Provide time for data write to execute

PUB SHIFT_OUT_TO_OLED(dataOut)
'// Low level routine that clocks in the serial command/data to the diaplay
'// Data is clocked into display on the rising edge of the Clock
'// 10 bits of data must be clocked into the display.

  dataOut ><= 10             ' reverse the lowest 10 bits
  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' drop clock
  OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
  dataOut >>= 1
  OUTA[SHARED_CLK] := 1                         ' raise clock

  OUTA[SHARED_CLK] := 0                         ' leave clock low on exit
  OUTA[SHARED_MOSI] := 0                         ' leave MOSI low on exit

PUB OLED_WRITE_LINE1(stringPtr)
'// Write character arry data to the first line of the display.

  OLED_COMMAND(%1000_0000)       ' Set DDRAM address to beginning of line1
  repeat strsize(stringPtr)
    OLED_DATA(byte[stringPtr++]) ' Drive characters onto the display

PUB OLED_WRITE_LINE2(stringPtr)
'// Write character arry data to the second line of the display.

  OLED_COMMAND(%1100_0000)       ' DDRAM address to beginning of line2
  repeat strsize(stringPtr)
    OLED_DATA(byte[stringPtr++]) ' Drive characters onto the display

PUB SET_MUXES(chanA, chanB, chanC, chanD) | muxVal
' method that sets the muxes to internal or external for each of the 4 channels in one sweep
' values passed in should be either 0 or 1 0 for internal and 1 for external
  muxVal := (chanD << 3) | (chanC << 2) | (chanB << 1) | (chanA)
  expVal1 &= %1111_0000         ' make sure lowest 4 bits are zeros; preserve the upper 4 bits
  expVal1 |= muxVal             ' OR in the nex mux values
  EXPANDER_WRITE(EXPANDER_1, expVal1)                   ' write out the values
  
PUB EXPANDER_WRITE(deviceAddress, expanderValue)

  case deviceAddress
    EXPANDER_1 : expVal1 := expanderValue
    EXPANDER_2 : expVal2 := expanderValue
    OTHER      : return MY_FALSE

  I2C.START
  I2C.WRITE(deviceAddress)
  I2C.WRITE(expanderValue)
  I2C.STOP
  return MY_TRUE

{PRI SHUTDOWN_ADC | dataOut
' ADC can be shutdown via software.  Clock shutdown command to ADC
'          call    #LOWER_CS                             ' lower CS line
'          mov     data,         #WCR                    ' put the address into outLong
'          call    #WRITE_BYTE                           ' Send it
'          
'          mov     data,         #%0_1_0_0_0_0_00        ' reset to be written to control register     
'          call    #WRITE_BYTE                           ' Send it
'          call    #RAISE_CS                             ' raise CS line
          
  DIRA[ADC_DRDYOUT] := 0
  DIRA[ADC_MISO]    := 0    
  DIRA[ADC_MOSI]    := 1    
  DIRA[ADC_SCLK]    := 1    
  DIRA[ADC_CS]      := 1        ' this device is still powered so hold in reset    
  OUTA[ADC_CS]      := 1        ' hold CS high for reset    

  dataOut := %0110_0000  ' Write Configuration Register
  dataOut ><= 8          ' reverse the order of these 8 bits
  repeat 8
    OUTA[ADC_SCLK] := 0                         ' drop clock
    OUTA[ADC_MOSI] := (dataOut & %01)           ' place next bit on MOSI
    dataOut >>= 1
    OUTA[ADC_SCLK] := 1                         ' raise clock
  OUTA[ADC_SCLK] := 0                         ' leave clock low on exit
  OUTA[ADC_MOSI] := 0                         ' leave MOSI low on exit

  dataOut := %1000_0000  ' set SHDN high to put device into shutdown
  dataOut ><= 8          ' reverse the order of these 8 bits
  repeat 8
    OUTA[ADC_SCLK] := 0                         ' drop clock
    OUTA[ADC_MOSI] := (dataOut & %01)           ' place next bit on MOSI
    dataOut >>= 1
    OUTA[ADC_SCLK] := 1                         ' raise clock
  OUTA[ADC_SCLK] := 0                         ' leave clock low on exit
  OUTA[ADC_MOSI] := 0                         ' leave MOSI low on exit

  DIRA[ADC_DRDYOUT] := 0
  DIRA[ADC_MISO]    := 0    
  DIRA[ADC_MOSI]    := 0    
  DIRA[ADC_SCLK]    := 0    
  DIRA[ADC_CS]      := 1        ' this device is still powered so hold in reset    
  OUTA[ADC_CS]      := 1        ' hold CS high for reset    
}

PUB READ_EUI 
  I2C.START
  I2C.WRITE(EUI_ADDR|I2C_WRITE)
  I2C.WRITE(EUI_BASE)
  I2C.START
  I2C.WRITE(EUI_ADDR|I2C_READ)
  eui_48[0] := I2C.READ(ACK)
  eui_48[1] := I2C.READ(ACK)
  eui_48[2] := I2C.READ(ACK)
  eui_48[3] := I2C.READ(ACK)
  eui_48[4] := I2C.READ(ACK)
  eui_48[5] := I2C.READ(NAK)
  I2C.STOP
  
  return @eui_48

PUB READ_RTC_SRAM | idx
' method that reads all the bytes from SRAM and stores them locally @rtcSram
  bytefill(@rtcSram, 0, 64)
  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_WRITE)
  I2C.WRITE(RTC_SRAM_BASE)
  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_READ)
  repeat idx from 0 to 62
    rtcSram[idx] := I2C.READ(ACK)
  rtcSram[63] := I2C.READ(NAK)
  I2C.STOP
  
  return @rtcSram

PUB WRITE_RTC_SRAM(sramPtr) | idx
  bytemove(@rtcSram, sramPtr, 64)       ' copy the parameters to local storage
  
  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_WRITE)
  I2C.WRITE(RTC_SRAM_BASE)
  repeat idx from 0 to 63
    I2C.WRITE(rtcSram[idx])
  I2C.STOP
  
PUB SET_RTC_TIME(year, month, day, hour, minute, second, wkday)
' stop the oscillator
' write minutes to minutes register (0x01)
' write hours to hours register (0x02)
' write wkday to wkday register (0x03) enable backup battery
' write date to date register (0x04)
' write month to month register (0x05)
' write year to year register (0x06)
' write seconds to seconds register (0x00) and enable ST bit
' RTCSEC         = $00
' RTCMIN         = $01
' RTCHOUR        = $02
' RTCWKDAY       = $03
' RTCDATE        = $04
' RTCMTH         = $05
' RTCYEAR        = $06

  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_WRITE)
  I2C.WRITE(RTCMIN)
  I2C.WRITE(CONVERT_TO_BCD(minute))
  I2C.WRITE(CONVERT_TO_BCD(hour))
  I2C.WRITE(CONVERT_TO_BCD(wkday)|VBATEN)
  I2C.WRITE(CONVERT_TO_BCD(day))
  I2C.WRITE(CONVERT_TO_BCD(month))
  I2C.WRITE(CONVERT_TO_BCD(year//100))
  I2C.STOP

  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_WRITE)
  I2C.WRITE(RTCSEC)
  I2C.WRITE(CONVERT_TO_BCD(second)|ST)
  I2C.IDLE
  I2C.STOP

PUB CONVERT_TO_BCD(number) : bcdValue
  number &= $FF        ' limit this number to a single byte
  bcdValue := ((number/10) << 4)
  bcdValue |= number//10

PUB CONVERT_FROM_BCD(bcdValue) : number
  number := (bcdValue >> 4)*10
  number += bcdValue & $0F

PUB READ_RTC_TIME
  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_WRITE)
  I2C.WRITE(RTCSEC)
  I2C.START
  I2C.WRITE(RTC_ADDR|I2C_READ)
  rtcTime[0] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0111_1111)   ' rtcsec
  rtcTime[1] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0111_1111)   ' rtcmin
  rtcTime[2] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0011_1111)   ' rtchour
  rtcTime[3] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0000_0111)   ' rtcwkday
  rtcTime[4] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0011_1111)   ' rtcdate
  rtcTime[5] := CONVERT_FROM_BCD(I2C.READ(ACK) & %0001_1111)   ' rtcmonth
  rtcTime[6] := CONVERT_FROM_BCD(I2C.READ(NAK) & %1111_1111)   ' rtcyear
  I2C.STOP
  return @rtcTime
  
PUB READ_EEPROM_LONG(startAddress) | eepromData
  ' read long
  return I2C.READ_LONG(EEPROM_ADDR, startAddress)

PUB WRITE_EEPROM_LONG(startAddress, val)
  I2C.WRITE_LONG(EEPROM_ADDR, startAddress, val)
  
PUB WRITE_TO_DAC(newDacValue)| dacCode
' method that clocks out 24 bits to the DAC.  This command is a "Write to and Update" variety
  dacCode := WRITE_AND_UPDATE | (newDacValue & $FFFF)
  dacCode ><= 24          ' reverse the 24 bits of interest with MSB now in bit 0

  OUTA[DAC_CS]  := 0                                    ' lower CS line
  repeat 24
    OUTA[SHARED_CLK] := 0                                    ' drop clock
    OUTA[SHARED_MOSI] := (dacCode & %01)                      ' place next bit on MOSI
    dacCode >>= 1                                            ' shift right by one
    OUTA[SHARED_CLK] := 1                                    ' raise clock
         
  OUTA[SHARED_MOSI] := 0
  OUTA[SHARED_CLK] := 0

  OUTA[DAC_CS]  := 1                                    ' raise CS line


PUB LSM_INIT | lsmPresent
  lsmPresent := 0
  ' setup accelerometer
  WRITE_LSM_REG(ACC_ADDR, LSM303_CTRL_REG4_A,  HR) 
  lsmPresent := READ_LSM_REG(ACC_ADDR, LSM303_CTRL_REG4_A)
  PAUSE_MS(1)

  ' big endian (bit 6 =0)
  ' +-2g (bit4:5 = 0)
  ' High resolution (bit 3 =1)
  WRITE_LSM_REG(ACC_ADDR, LSM303_CTRL_REG1_A, ORD2 | ORD0 | LPEN | XEN | YEN | ZEN) ' set to 100Hz; low power, Z,Y,X
  lsmPresent := READ_LSM_REG(ACC_ADDR, LSM303_CTRL_REG1_A)
  PAUSE_MS(1)

  
  ' setup magnetometer - continuous conversion
  WRITE_LSM_REG(MAG_ADDR, LSM303_MR_REG_M, $00)
'  lsmPresent := READ_LSM_REG(MAG_ADDR, LSM303_MR_REG_M)
  PAUSE_MS(1)

  ' 1.5Hz data rate
  WRITE_LSM_REG(MAG_ADDR, LSM303_CRA_REG_M, TEMP_EN | DO30_0)
  lsmPresent := READ_LSM_REG(MAG_ADDR, LSM303_CRA_REG_M)
  PAUSE_MS(1)

  ' +-1.3gauss
  WRITE_LSM_REG(MAG_ADDR, LSM303_CRB_REG_M, GN1_3)
  PAUSE_MS(1)
  return lsmPresent

PRI WRITE_LSM_REG(deviceAddress, registerAddress, data)
  I2C.START
  I2C.WRITE((deviceAddress<<1)|I2C_WRITE)
  I2C.WRITE(registerAddress)
  I2C.WRITE(data)
  I2C.STOP

PRI READ_LSM_REG(deviceAddress, registerAddress) : response
  I2C.START
  I2C.WRITE((deviceAddress<<1)|I2C_WRITE)
  I2C.WRITE(registerAddress)
  'I2C.STOP
  I2C.START
  I2C.WRITE((deviceAddress<<1)|I2C_READ)
  response := I2C.READ(NAK)
  I2C.STOP

PUB GET_ACC_X : xAcc
  xAcc := READ_LSM_REG(ACC_ADDR, LSM303_OUT_X_H_A) << 24 ' put this into the MSB
  xAcc |= READ_LSM_REG(ACC_ADDR, LSM303_OUT_X_L_A) << 16 ' put this into the MSB-1
  xAcc ~>= (16+4)               ' shift everything back right to get 12-bit result with sign preserved.    
  return 
  
PUB GET_ACC_Y : yAcc
  yAcc := READ_LSM_REG(ACC_ADDR, LSM303_OUT_Y_H_A) << 24 ' put this into the MSB
  yAcc |= READ_LSM_REG(ACC_ADDR, LSM303_OUT_Y_L_A) << 16 ' put this into the MSB-1
  yAcc ~>= (16+4)               ' shift everything back right to get 12-bit result with sign preserved.    

PUB GET_ACC_Z : zAcc
  zAcc := READ_LSM_REG(ACC_ADDR, LSM303_OUT_Z_H_A) << 24 ' put this into the MSB
  zAcc |= READ_LSM_REG(ACC_ADDR, LSM303_OUT_Z_L_A) << 16 ' put this into the MSB-1
  zAcc ~>= (16+4)               ' shift everything back right to get 12-bit result with sign preserved.    

PUB READ_MAG 
  I2C.START
  I2C.WRITE(MAG_ADDR<<1|I2C_WRITE)
  I2C.WRITE(LSM303_OUT_X_H_M)
  I2C.START
  I2C.WRITE(MAG_ADDR<<1|I2C_READ)
  mag[0] := I2C.READ(ACK)
  mag[1] := I2C.READ(ACK)
  mag[2] := I2C.READ(ACK)
  mag[3] := I2C.READ(ACK)
  mag[4] := I2C.READ(ACK)
  mag[5] := I2C.READ(NAK)
  I2C.STOP
  
PUB GET_MAG_X : magX
  magX := mag[0] << 24 | mag[1] << 16
  magX ~>= 16 
  
PUB GET_MAG_Z : magZ
  magZ := mag[2] << 24 | mag[3] << 16
  magZ ~>= 16 

PUB GET_MAG_Y : magY
  magY := mag[4] << 24 | mag[5] << 16
  magY ~>= 16 

PUB READ_MAG_TEMP : magTemp 
' temperature is 12 bits in the top bits of a 16 bit h,l word
' temp is 2s complement
  magTemp := READ_LSM_REG(MAG_ADDR, LSM303_TEMP_OUT_H_M) << 24 ' put this into the MSB
  magTemp |= READ_LSM_REG(MAG_ADDR, LSM303_TEMP_OUT_L_M) << 16 ' put this into the MSB-1
  magTemp ~>= (16)               ' shift everything back right to get 12-bit result with sign preserved.    


PUB EXPANDER_TOGGLE_BIT(deviceAddress, bitName)
' this method requires EXPANDER_1 or EXPANDER_2 for the device address
' and the BITNAME not the bit number.  It expects the bit to be deocded already
' for instance 5V_EN rather than 2 for the bit number.                        
  if deviceAddress == EXPANDER_1
    expVal1 ^= bitName 
    return EXPANDER_WRITE(deviceAddress, expVal1)

  if deviceAddress == EXPANDER_2
    expVal2 ^= bitName 
    return EXPANDER_WRITE(deviceAddress, expVal2)

  return FALSE
      
{PUB WAIT_FOR_TIME(modValue)
  repeat
    PAUSE_MS(700)
    READ_RTC_TIME
  until (rtcTime[1]//modValue)==(modValue - 1) AND rtcTime[0] == 40
  
  '_rc_to_fast_prop
  _slow_to_fast_prop
  I2C_INIT
  OLED_ON
  OLED_WRITE_LINE1(string("Waking System."))
}
PUB TRIGGER_START(interval) | lastRTCTime
' method that checks the current RTC time and returns true if we are within 20 seconds of the start minute
  if (CNT - lastRTCTime) > clkfreq                      ' avoid checking the RTC too often
    READ_RTC_TIME
    lastRTCTime := CNT
    if (rtcTime[1]//interval)==(interval - 1) AND (rtcTime[0] > 40)
      return TRUE              
  return FALSE
  
PUB TRIGGER_END(recordLength, interval)
' method that watches the time and returns true when the current time equals the record lengh-
' this means we've recorded what we should and should now turn off
  if ((rtcTime[1]//interval) == 0) AND (rtcTime[0] > recordLength)
    return TRUE             
    
PUB SWITCHED_ON(sleepPress, continuousPress, interval) 
{
  if INA[REED_SWITCH] == 0      ' is button "pressed"?
    waitcnt(clkfreq/1000 + cnt) ' wait for 1/10s debounce
    if INA[REED_SWITCH] == 0    ' is button still "pressed"
      return TRUE               ' if button is pressed return immediately
                                ' otherwise check clock to see if we should wake up
}

' is it time for a trigger recording?
  READ_RTC_TIME
  if (rtcTime[1]//interval)==(interval - 1) AND rtcTime[0] > 40
    return 1              ' if we are within 20 seconds of the interval minute

' has there been a button press?
  repeat
    PAUSE_MS(1)  ' if it's pressed just wait a bit and return
  until INA[REED_SWITCH] == NOT_PRESSED

  case PHSA
    clkfreq>>4..sleepPress                 : 
      return 2
    continuousPress..continuousPress<<1    : 
      return 3

  return -1              ' otherwise it's not time to wake up so don't


PUB SWITCH_OFF(recordLength,second)
' this is the code for sleeping the system
' did someone press the button for a medium time?
  'if (PHSA > (clkfreq*5)) AND INA[REED_SWITCH] == NOT_PRESSED
  '  return 0                 ' time to shutdown

  if recordLength == 0          ' recordLength == 0 means we are in continuous mode
    return 1

  if second == recordLength     ' if the current second is the same as the record length
    return 2                 ' then we should shutdown  


PUB SHORT_INPUTS(ch)
' this method shorts the inputs on the given channel and sets gain to 0

  WRITE_PGA(ch, %0100_0001)

PUB SET_GAIN(_gain, ch) | gain, response, gainRegister
'' method that sets the gain.  Gain is set for channel ch

  case _gain
     0       : response := %0010_0000                   ' short inputs
               gain     :=     0                        ' indicate channel is shorted
     1..5    : response := %0000
               gain     :=     1
     6..15   : response := %0001
               gain     :=    10
     16..25  : response := %0010
               gain     :=    20
     26..35  : response := %0011
               gain     :=    30
     36..50  : response := %0100
               gain     :=    40
     51..70  : response := %0101
               gain     :=    60
     71..90  : response := %0111
               gain     :=    80
     other   : response := %0000
               gain     :=     1


  gainRegister := (response << 1 | $01) ' be sure bit zero is 1 for gain write
   
  WRITE_PGA(ch, gainRegister)        ' set gain register

  return gain

PUB GAIN_BINARY_VAL(gain) | response
'' method that decodes the gain enum settings

  case gain
     0       : response := 0 'shorted inputs
     1..5    : response := 1 '%0001
     6..15   : response := 2 '%0010
     16..25  : response := 3 '%0011
     26..35  : response := 4 '%0100
     36..50  : response := 5 '%0101
     51..70  : response := 6 '%0110
     71..90  : response := 7 '%0111
     other   : response := %0000


  return response

PUB SHUTDOWN_ADC : response
' method that clocks out (in spin) the commands necessary to shutdown the adc
  ' Setup pins
  OUTA[ADC_CS]   := 1
  DIRA[ADC_CS]   := 1

  OUTA[ADC_SCLK] := 1
  DIRA[ADC_SCLK] := 1
  
  OUTA[ADC_MOSI] := 0
  DIRA[ADC_MOSI] := 1
                           
  WRITE_OPERATION(%0110_0000, %1_0_0_0_0_1_00, 1)        ' hold shutdown bit high indicating shutdown
  'WRITE_OPERATION(%0110_0000, %0_1_0_0_0_1_00, 1)
  'WRITE_OPERATION(%0110_0000, %0_0_1_0_0_1_00, 1)
  response := READ_REGISTER(%1110_0000, 1)

  OUTA[ADC_DRDYOUT] := 0
  DIRA[ADC_DRDYOUT] := 1
  OUTA[ADC_MISO]    := 0
  DIRA[ADC_MISO]    := 1
  OUTA[ADC_MOSI]    := 0
  DIRA[ADC_MOSI]    := 1
  OUTA[ADC_SCLK]    := 0
  DIRA[ADC_SCLK]    := 1
  OUTA[ADC_CS]      := 0
  DIRA[ADC_CS]      := 1

PUB WRITE_OPERATION(address, data, bytesToWrite)
  OUTA[ADC_CS] := 0     ' lower ADC_CS line

  WRITE_COMMAND(address)
  WRITE_DATA(data, bytesToWrite) 
   
  OUTA[ADC_CS] := 1     ' raise ADC_CS line
   
PUB WRITE_COMMAND(address)
  WRITE_DATA(address, 1)
 
PUB WRITE_DATA(data, count) | outData, bits
  case count
    1 : bits    := 8
        outData := data >< bits   ' reverse order of lowest 8 bits
    2 : bits    := 16
        outData := data >< bits   ' reverse order of lowest 16 bits
    4 : bits    := 32
        outData := data >< bits   ' reverse order of all 32 bits
        

  '  all bits have been reversed so now we can shift them out by shifting to the right.  
  repeat bits     ' how many bits are we sending?
    OUTA[ADC_SCLK] := 1                      ' raise clock
    OUTA[ADC_MOSI] := outData & %0000_0001   ' output bit
    OUTA[ADC_SCLK] := 0                      ' lower clock
    outdata >>= 1

PUB READ_REGISTER(address, count) | inData
  inData := 0
  OUTA[ADC_CS] := 0     ' lower ADC_CS line
  WRITE_COMMAND(address)

  repeat count*8  ' we need 8 bits for each byte
    OUTA[ADC_SCLK] := 1
    inData := (inData << 1) + INA[ADC_MISO]
    OUTA[ADC_SCLK] := 0
    
  OUTA[ADC_CS] := 1    ' raise ADC_CS line
  return inData

PRI WRITE_PGA(ch, pgaData) | tmp
' ch should be %0001-%1000, pgaData should be data to be clocked out to PGA
' writing to the PGA is pretty simple.  It's always 8 bits CPOL=CPHA=0; LSB first
' see page 8 of the data sheet

  OUTA[SHARED_CLK] := 0        ' set clock low
  OUTA[SHARED_MOSI] := 0        ' set MOSI low
  DIRA[SHARED_CLK] := 1        ' set clock to output
  DIRA[SHARED_MOSI] := 1        ' set MOSI to output

  ' lower appropriate CS line
  expVal1 &= (!ch<<4)
  EXPANDER_WRITE(EXPANDER_1, expVal1)
  
  repeat 8     ' how many bits are we sending?
    OUTA[SHARED_MOSI] := pgaData & %0000_0001   ' output bit (send LSB first)
    OUTA[SHARED_CLK] := 1                      ' raise clock  
    pgadata >>= 1
    OUTA[SHARED_CLK] := 0                      ' lower clock  
  OUTA[SHARED_MOSI] := 0 ' lower MOSI at end

  ' raise the CS line to latch the command
  expVal1 |= PGA_D_CS | PGA_C_CS | PGA_B_CS | PGA_A_CS '%1111_0000
  EXPANDER_WRITE(EXPANDER_1, expVal1)


PUB TO_HEX(value)
'' Print a hexadecimal number
  value <<= 7 << 2
  return lookupz((value <-= 4) & $F : "0".."9", "A".."F")
  
PRI INIT_A0_B0_C0 ' not really needed since this is just an empty space                                                    
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($40)             ' Set CGRAM start address
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  

PRI INIT_A0_B0_C1                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($48)             ' Set CGRAM start address
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  

PRI INIT_A0_B1_C0                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($50)             ' Set CGRAM start address
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  

PRI INIT_A0_B1_C1                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($58)             ' Set CGRAM start address
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  

PRI INIT_A1_B0_C0                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($60)             ' Set CGRAM start address
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  

PRI INIT_A1_B0_C1                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($68)             ' Set CGRAM start address
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  

PRI INIT_A1_B1_C0                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($70)             ' Set CGRAM start address
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  

PRI INIT_A1_B1_C1                                                     
'// Create the custom character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.
   OLED_COMMAND($78)             ' Set CGRAM start address
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%0_0000)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  
   OLED_DATA(%1_1111)            ' Write Data to CGRAM  

PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

DAT ' characters for OLED
PLOT_1 byte   %000_11111 
       byte   %000_11111
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111

PLOT_2 byte   %000_11111
       byte   %000_11111
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
              
PLOT_3 byte   %000_00000 
       byte   %000_00000
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
              
PLOT_4 byte   %000_11111 
       byte   %000_11111
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
              
PLOT_5 byte   %000_00000 
       byte   %000_00000
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
              
PLOT_6 byte   %000_00000 
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_00000
       byte   %000_11111
       byte   %000_11111
  