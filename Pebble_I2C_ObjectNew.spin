{Pebble_I2C_ObjectNew}
' Replaced the I2C object based on Ray Allen's I2C code with code from Jonny Mac.
' Ray's code didn't get along with the fuel gauge because there were problems
' with repeated Start conditions.

' PGB
' 20 Mar 2015
' PSICE
'' =================================================================================================
''
''   File....... jm_i2c.spin
''   Purpose.... Low/Mid-level I2C routines
''   Author..... Jon "JonnyMac" McPhalen
''               Copyright (c) 2009-2012 Jon McPhalen
''               -- elements inspired by code from Mike Green
''               -- see below for terms of use
''   E-mail..... jon@jonmcphalen.com
''   STARTed.... 28 JUL 2009
''   UpDATed.... 15 JUN 2012
''
'' =================================================================================================

{{
 
   Assumes that SCL and SDA are fixed in a given application and can be set on
   initialization of the object.  Code does not drive SCL or SDA pins high; pull-ups
   are required on both pins.

   Use "x" routines for a device with that uses a 16-bit address (e.g., 32k/64k EEPROM)
  
}}   


CON

   #0, ACK, NAK

DAT

scl             byte    -1                                      ' clock pin of i2c buss
sda             byte    -1                                      ' DATa pin of i2c buss

defined         byte    FALSE

PUB INIT(sclpin, sdapin)

'' Define I2C SCL (clock) and SDA (DATa) pins
'' -- will not redefine pins once defined
''    * assumes all I2C devices on same pins

  if defined
    return
    
  longmove(@scl, @sclpin, 2)                                  '  copy pins
  DIRA[scl] := 0                                              '  float to pull-up
  OUTA[scl] := 0                                              '  WRITE 0 to output reg
  DIRA[sda] := 0
  OUTA[sda] := 0

  repeat 9                                                      ' reset device
    DIRA[scl] := 1
    DIRA[scl] := 0
    if (INA[sda])
      quit

  defined := TRUE
  
PUB CLOSE

'' Clear I2C pin definitions

  longfill(@scl, -1, 2)                                     ' undefine pins
  DIRA[scl] := 0                                            ' force to inputs
  DIRA[sda] := 0
  defined := FALSE

CON

  { ================================= }
  {                                   }
  {  8 - B I T   A D D R E S S I N G  }
  {                                   }
  { ================================= }
  

PUB WR_BYTE(id, addr, value) | ackbit

'' WRITE byte to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  ackbit := WR_PAGE(id, addr, 1, @value)

  return ackbit


PUB WR_WORD(id, addr, value) | ackbit

'' WRITE word to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device  

  ackbit := WR_PAGE(id, addr, 2, @value)

  return ackbit


PUB WR_LONG(id, addr, value) | ackbit

'' WRITE long to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  ackbit := WR_PAGE(id, addr, 4, @value)

  return ackbit
   

PUB WR_PAGE(id, addr, n, src) | ackbit

'' WRITE n bytes from src to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' --    n : bytes to WRITE
'' --  src : pointer to source value(s)
''
'' Be mindful of address/page size in device to prevent page wrap-around

  WAIT(id)
  WRITE(addr)
  ackbit := ACK                                                 ' assume okay
  repeat n
    ackbit |= WRITE(byte[src++])                                ' WRITE a byte 
  stop

  return ackbit


PUB WR_STR(id, addr, src) | ackbit, b

'' WRITE (arbitrary-length) z-string at src to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' --  src : pointer to z-string
''
'' Allows string to cross device page boundaries

  ackbit := ACK                                                 ' assume okay  
  repeat (strsize(src) + 1)                                     ' WRITE string + 0
    ackbit |= WR_BYTE(id, addr++, byte[src++])

  return ackbit
     

PUB RD_BYTE(id, addr) | value

'' Read byte from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  RD_PAGE(id, addr, 1, @value)
  
  return value & $FF


PUB RD_WORD(id, addr) | value

'' Read word from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  RD_PAGE(id, addr, 2, @value)

  return value & $FFFF


PUB RD_LONG(id, addr) | value

'' Read long from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  RD_PAGE(id, addr, 4, @value)

  return value 


PUB RD_PAGE(id, addr, n, dest)

'' Read n bytes from device w/8-bit addressing; output to dest
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' --    n : bytes to read
'' -- dest : pointer to the destINAtion
''
'' Be mindful of address/page size in device to prevent page wrap-around

  WAIT(id)
  WRITE(addr)
  START                                                         ' reSTART for read
  WRITE(id | $01)                                               ' set read bit
  repeat (n - 1)
    byte[dest++] := read(ACK)
  byte[dest] := read(NAK)                                       ' last byte gets NAK
  stop 

  
PUB RD_STR(id, addr, dest) | b

'' Read (arbitrary-length) z-string, store at dest
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' -- dest : pointer to the destINAtion
''
'' Allows string to cross device page boundaries

  repeat
    b := RD_BYTE(id, addr++)                                    ' read byte from card
    byte[dest++] := b                                           ' WRITE to string  
    if (b == 0)                                                 ' at end?
      quit                                                      '  if yes, we're done
  

CON

  { ===================================== }
  {                                       }
  {  L O W   L E V E L   R O U T I N E S  }
  {                                       }
  { ===================================== }
  
        
PUB WAIT(id) | ackbit

'' WAITs for I2C device to be ready for new command

  repeat
    START
    ackbit := WRITE(id & $FE)
  until ackbit == ACK


PUB START

'' Create I2C START sequence
'' -- will WAIT if I2C buss SDA pin is held low

  DIRA[sda] := 0                                                ' float SDA (1)
  DIRA[scl] := 0                                                ' float SCL (1)
  repeat while (INA[scl] == 0)                                  ' allow "clock stretching"

  OUTA[sda] := 0
  DIRA[sda] := 1                                                ' SDA low (0)

  
PUB WRITE(i2cbyte) | ackbit

'' WRITE byte to I2C buss

  OUTA[scl] := 0
  DIRA[scl] := 1                                                ' SCL low

  i2cbyte <<= CONSTANT(32-8)                                    ' move msb (bit7) to bit31
  repeat 8                                                      ' output eight bits
    DIRA[sda] := ((i2cbyte <-= 1) ^ 1)                          ' send msb first
    DIRA[scl] := 0                                              ' SCL high (float to p/u)
    DIRA[scl] := 1                                              ' SCL low

  DIRA[sda] := 0                                                ' relase SDA to read ack bit
  DIRA[scl] := 0                                                ' SCL high (float to p/u)  
  ackbit := INA[sda]                                            ' read ack bit
  DIRA[scl] := 1                                                ' SCL low

  return (ackbit & 1)


PUB READ(ackbit) | i2cbyte

'' Read byte from I2C buss

  OUTA[scl] := 0                                                ' prep to WRITE low
  DIRA[sda] := 0                                                ' make input for read

  repeat 8
    DIRA[scl] := 0                                              ' SCL high (float to p/u)
    i2cbyte := (i2cbyte << 1) | INA[sda]                        ' read the bit
    DIRA[scl] := 1                                              ' SCL low
                             
  DIRA[sda] := !ackbit                                          ' output ack bit 
  DIRA[scl] := 0                                                ' clock it
  DIRA[scl] := 1

  return (i2cbyte & $FF)


PUB STOP

'' Create I2C stop sequence 

  OUTA[sda] := 0
  DIRA[sda] := 1                                                ' SDA low
  
  DIRA[scl] := 0                                                ' float SCL
  repeat while (INA[scl] == 0)                                  ' hold for clock stretch
  
  DIRA[sda] := 0                                                ' float SDA


DAT

{{

  Terms of Use: MIT License

  Permission is hereby granted, free of charge, to any person obtaining a copy of this
  software and associated documentation files (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy, modify,
  merge, PUBlish, distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to the following
  CONditions:

  The above copyright notice and this permission notice shall be included in all copies
  or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
  OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

}}