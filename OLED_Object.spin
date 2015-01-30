{OLED_Object
Object written to run the NHD-0216KZW-AB5 2x16 character display
This object is based on K. Culkins example C-code (v1.00) written for a
microchip processor.  

I've adapted it for the prop/spin
PGB
2 Dec 2014
Christchurch, NZ
}
CON ' Clock mode settings
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  CLK_FREQ = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS   = CLK_FREQ / 1_000                                   ' ticks in 1ms
  ONE_US   = CLK_FREQ / 1_000_000                               ' ticks in 1us
 
CON ' OLED CONSTANTS
'  CLEAR_DISPLAY   = %00_0000_0001
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

  BACK_SLASH      = $00
  VERT_LINE       = $01
  CLEAR_OPEN      = $02
  CLEAR_CLOSE     = $03
  T1_M1_B1        = $04
  T1_M1_B0        = $05
  
  SPACE           = $20
  FORWARD_SLASH   = $2F
  DASH            = $2D

CON ' Shared SPI bus pins
  SHARED_MOSI = 22
  SHARED_MISO = 23
  SHARED_SCLK = 21

  OLED_CS   = 24  ' OLED CHIP SELECT pin
  DAC_CS    = 25  ' DAC CHIP SELECT pin
  RADIO_CS  = 26
  

OBJ
  PEBBLE    : "BasicPebbleFunctions"                ' I put these into a seperate file to make editing easier

DAT              '0123456789012345
  lcd_text  byte "                ", 0
VAR
  long  symbol
   
PUB MAIN
  PEBBLE.INIT
  PEBBLE.OLED_ON_ONLY

' setup the shared SPI
  DIRA[RADIO_CS] := 1
  DIRA[OLED_CS]  := 1
  DIRA[DAC_CS]   := 1
  
  OUTA[RADIO_CS] := 1
  OUTA[OLED_CS]  := 1           ' current appears to leak from this device.  Since it's powered off don't worry about CS lineS
  OUTA[DAC_CS]   := 1

  DIRA[SHARED_MOSI] := 1
  OUTA[SHARED_MOSI] := 0

  DIRA[SHARED_MISO] := 0
  
  DIRA[SHARED_SCLK] := 1
  OUTA[SHARED_SCLK] := 1       ' this is set to 1 and I'm not sure why; just testing it


  INIT_OLED

  ' now create the four custom characters
  INIT_BACK_SLASH
  INIT_VERT_LINE
  INIT_CLEAR_OPEN
  INIT_CLEAR_CLOSE
  INIT_T1_M1_B1
  INIT_T1_M1_B0

  LCD_COMMAND($0C)              ' Turn ON OLED
  repeat
    bytemove(@lcd_text, string("Newhaven OLED..."), 16)
    LCD_Write_line1(@lcd_text)
    
    bytemove(@lcd_text, string("From Digi-key"), 13)
    LCD_Write_line2(@lcd_text)

    Animate_wait

    Clear_display
    
PUB Animate_wait | y
  y := $CF
  repeat 8
    LCD_Command(y)              ' set cursor position; end of second line
    LCD_DATA(FORWARD_SLASH)     ' write data to DDRAM
    PAUSE_MS(25)
    
    LCD_Command(y)              ' set cursor position; end of second line
    LCD_DATA(DASH)              ' write data to DDRAM
    PAUSE_MS(25)
    
    LCD_Command(y)              ' set cursor position; end of second line
    LCD_DATA(BACK_SLASH)        ' write data to DDRAM
    PAUSE_MS(25)
    
    LCD_Command(y)              ' set cursor position; end of second line
    LCD_DATA(VERT_LINE)         ' write data to DDRAM
    PAUSE_MS(25)
    
  LCD_Command(y)                ' set cursor position; end of second line
  LCD_Data($2A)                 ' Asterisk

PUB Clear_display | x, y
  x := 0
  repeat 16
    LCD_Command($80+x)          ' set cursor position, first character, first line
    LCD_Data(CLEAR_OPEN)        ' write data cgram
    PAUSE_MS(500)                '  

    LCD_Command($80+x)          ' set cursor position, first character, first line
    LCD_Data(CLEAR_CLOSE)        ' write data cgram
    PAUSE_MS(500)                '  

    LCD_Command($80+x)          ' set cursor position, first character, first line
    LCD_Data(SPACE)             ' write data cgram

    LCD_Command($80+x)          ' set cursor position, first character, first line
    LCD_Data(T1_M1_B1)             ' write data cgram
    x++
    
  x := 0
  repeat 16
    LCD_Command($C0+x)          ' set cursor position, first character, second line
    LCD_Data(CLEAR_OPEN)        ' write data cgram
    PAUSE_MS(500)                '  

    LCD_Command($C0+x)          ' set cursor position, first character, second line
    LCD_Data(CLEAR_CLOSE)        ' write data cgram
    PAUSE_MS(500)                '  

    LCD_Command($C0+x)          ' set cursor position, first character, second line
    LCD_Data(SPACE)             ' write data cgram

    LCD_Command($C0+x)          ' set cursor position, first character, first line
    LCD_Data(T1_M1_B0)             ' write data cgram
    x++
  
PUB INIT_OLED

  LCD_COMMAND($38)   ' Function Set
  LCD_COMMAND($08)   ' Display OFF
  LCD_COMMAND($01)   ' Display Clear
  PAUSE_MS(1)         ' Additional delay to allow Display Clear to complete
  LCD_COMMAND($06)   ' Entry Mode Set
  LCD_COMMAND($02)   ' Home Command

PUB Init_back_slash
'// Create the custom back slash character in CGRAM.
'// DDRAM address in Font Table is $00.
'// Address location increments automatically after each write to CGRAM.

   LCD_COMMAND($40)'; // Set CGRAM start address
   LCD_Data($00)';    // Write Data to CGRAM
   LCD_Data($10)';    // Write Data to CGRAM
   LCD_Data($08)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($02)';    // Write Data to CGRAM
   LCD_Data($01)';    // Write Data to CGRAM
   LCD_Data($00)';    // Write Data to CGRAM
   LCD_Data($00)';    // Write Data to CGRAM

PUB Init_vert_line
'// Create the custom vertical line character in CGRAM.
'// DDRAM address in Font Table is $01.
'// Address location increments automatically after each write to CGRAM.

   LCD_Command($48)'; // Set CGRAM start address
   LCD_Data($00)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($04)';    // Write Data to CGRAM
   LCD_Data($00)';    // Write Data to CGRAM
   LCD_Data($00)';    // Write Data to CGRAM

PUB Init_clear_open
'// Create the custom clear character "open" graphic in CGRAM.
'// DDRAM address in Font Table is $02.
'// Address location increments automatically after each write to CGRAM.

   LCD_Command($50)'; // Set CGRAM start address
   LCD_Data($03)';    // Write Data to CGRAM
   LCD_Data($06)';    // Write Data to CGRAM
   LCD_Data($0C)';    // Write Data to CGRAM
   LCD_Data($18)';    // Write Data to CGRAM
   LCD_Data($0C)';    // Write Data to CGRAM
   LCD_Data($06)';    // Write Data to CGRAM
   LCD_Data($03)';    // Write Data to CGRAM
   LCD_Data($00)';    // Write Data to CGRAM

PUB Init_clear_close
'// Create the custom clear character "close" graphic in CGRAM.
'// DDRAM address in Font Table is $03.
'// Address location increments automatically after each write to CGRAM.

   LCD_Command($58)'; // Set CGRAM start address
   LCD_Data($00)';    // Write Data to CGRAM     0_0000
   LCD_Data($00)';    // Write Data to CGRAM     0_0000
   LCD_Data($07)';    // Write Data to CGRAM     0_0111
   LCD_Data($1F)';    // Write Data to CGRAM     1_1111
   LCD_Data($07)';    // Write Data to CGRAM     0_0111
   LCD_Data($00)';    // Write Data to CGRAM     0_0000
   LCD_Data($00)';    // Write Data to CGRAM     0_0000
   LCD_Data($00)';    // Write Data to CGRAM     0_0000
                                                 
PUB INIT_T1_M1_B1
'// Create the custom clear character "close" graphic in CGRAM.
'// DDRAM address in Font Table is $04.
'// Address location increments automatically after each write to CGRAM.

   LCD_Command($60)'; // Set CGRAM start address
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0111
   LCD_Data(%1_1111)';    // Write Data to CGRAM     1_1111
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0111
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0000
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000

PUB INIT_T1_M1_B0
'// Create the custom clear character "close" graphic in CGRAM.
'// DDRAM address in Font Table is $05.
'// Address location increments automatically after each write to CGRAM.

   LCD_Command($68)'; // Set CGRAM start address
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0000
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0111
   LCD_Data(%1_1111)';    // Write Data to CGRAM     1_1111
   LCD_Data(%1_1111)';    // Write Data to CGRAM     0_0111
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0000
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0000
   LCD_Data(%0_0000)';    // Write Data to CGRAM     0_0000

PUB LCD_write_line1(stringPtr)
'// Write character arry data to the first line of the display.

  LCD_Command(%1000_0000)       ' Set DDRAM address
  repeat strsize(stringPtr)
    LCD_Data(byte[stringPtr++]) ' Drive characters onto the display

PUB LCD_write_line2(stringPtr)
'// Write character arry data to the second line of the display.

  LCD_Command(%1100_0000)       ' DDRAM address
  repeat strsize(stringPtr)
    LCD_Data(byte[stringPtr++]) ' Drive characters onto the display

PUB LCD_Command(Command)
'// Command Write
'// RS = 0 and RW = 0.
  OUTA[OLED_CS]     := 0   ' lower CS line to enable the display
  Serial_byte(Command)     ' Call the data byte driver function
  OUTA[OLED_CS]     := 1   ' raise CS line to disable the display
  PAUSE_MS(1)              ' Provide time for command to execute

PUB LCD_Data(RAM_data)
'// RAM Write
'// RS = 1 and RW = 0.
  OUTA[OLED_CS]     := 0   ' lower CS line to enable the display
  RAM_data |= $0200        ' Enable the RS bit
  Serial_byte(RAM_data)    ' Call the data byte driver function
  OUTA[OLED_CS]     := 1   ' raise CS line to disable the display
  PAUSE_MS(1)              ' Provide time for data write to execute

PUB Serial_byte(dataOut)
'// Low level routine that clocks in the serial command/data to the diaplay
'// Data is clocked into display on the rising edge of the Clock
'// 10 bits of data must be clocked into the display.

  dataOut ><= 10             ' reverse the lowest 10 bits
  repeat 10
    OUTA[SHARED_SCLK] := 0                         ' drop clock
    OUTA[SHARED_MOSI] := (dataOut & %01)           ' place next bit on MOSI
    dataOut >>= 1
    OUTA[SHARED_SCLK] := 1                         ' raise clock

PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

        