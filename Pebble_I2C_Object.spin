{Pebble_I2C_Object}
' Based on Ray Allen's I2C code used for running anything on the pebble/prop I2C bus
' PGB
' 29 Aug 2014
' PSICE
CON
   ACK      = 0                        ' I2C Acknowledge
   NAK      = 1                        ' I2C No Acknowledge
   Xmit     = 0                        ' I2C Direction Transmit
   Recv     = 1                        ' I2C Direction Receive
   BootPin  = 28                       ' I2C Boot EEPROM SCL Pin
   EEPROM   = $A0                      ' I2C EEPROM Device Address


VAR
  byte  _sdaPin, _sclPin
   
PUB INIT(sdaPin, sclPin)
' here we really only need to setup the pins

  _sdaPin := sdaPin
  _sclPin := sclPin

  return TRUE
  
PUB START                                  ' _sdaPin goes HIGH to LOW with _sclPin HIGH
   outa[_sclPin]~~                         ' Initially drive _sclPin HIGH
   dira[_sclPin]~~
   outa[_sdaPin]~~                         ' Initially drive _sdaPin HIGH
   dira[_sdaPin]~~
   outa[_sdaPin]~                          ' Now drive _sdaPin LOW
   outa[_sclPin]~                          ' Leave _sclPin LOW

PUB STOP'(_sclPin) | _sdaPin                    ' _sdaPin goes LOW to HIGH with _sclPin High
   outa[_sclPin]~~                         ' Drive _sclPin HIGH
   outa[_sdaPin]~~                         '  then _sdaPin HIGH
   dira[_sclPin]~                          ' Now let them float
   dira[_sdaPin]~                          ' If pullups present, they'll stay HIGH

PUB IDLE
   dira[_sclPin]~                          ' Now let them float
   dira[_sdaPin]~                          ' If pullups present, they'll stay HIGH

PUB WRITE(data) | ackbit '(_sclPin, data) : ackbit | _sdaPin
'' Write i2c data.  Data byte is output MSB first, _sdaPin data line is valid
'' only while the _sclPin line is HIGH.  Data is always 8 bits (+ ACK/NAK).
'' _sdaPin is assumed LOW and _sclPin and _sdaPin are both left in the LOW state.
   ackbit := 0 
   data <<= 24
   repeat 8                            ' Output data to _sdaPin
      outa[_sdaPin] := (data <-= 1) & 1' mask off lowest bit
      outa[_sclPin] := 1               ' raise clock
      outa[_sclPin] := 0               ' lower clock

   dira[_sdaPin] := 0                      ' Set _sdaPin to input for ACK/NAK
   outa[_sclPin] := 1
   ackbit := ina[_sdaPin]                  ' Sample _sdaPin when _sclPin is HIGH
   outa[_sclPin] := 0
   outa[_sdaPin] := 0                      ' Leave _sdaPin LOW
   dira[_sdaPin] := 1                      ' set as output so it's pulled low

PUB READ(ackbit):data'(_sclPin, ackbit): data | _sdaPin
'' Read in i2c data, Data byte is output MSB first, _sdaPin data line is
'' valid only while the _sclPin line is HIGH.  _sclPin and _sdaPin left in LOW state.
   data := 0
   dira[_sdaPin]~                          ' Make _sdaPin an input
   repeat 8                                ' Receive data from _sdaPin
      outa[_sclPin] := 1                   ' Sample _sdaPin when _sclPin is HIGH
      data := (data << 1) | ina[_sdaPin]
      outa[_sclPin]~
   outa[_sdaPin] := ackbit                 ' Output ACK/NAK to _sdaPin
   dira[_sdaPin] := 1
   outa[_sclPin] := 1                      ' Toggle _sclPin from LOW to HIGH to LOW
   outa[_sclPin] := 0
   outa[_sdaPin] := 0                      ' Leave _sdaPin driven LOW

PUB READ_LONG(devSel, addrReg) : data
'' Read in a single long of i2c data.  Device select code is devSel.  Device
'' starting address is addrReg.  The device select code is modified using the
'' upper 3 bits of the 19 bit addrReg.  This returns true if an error occurred.
'' Note that you can't distinguish between a return value of -1 and true error.
   if READ_PAGE(devSel, addrReg, @data, 4)
      return -1

PUB WRITE_LONG(devSel, addrReg, data)
'' Write out a single long of i2c data.  Device select code is devSel.  Device
'' starting address is addrReg.  The device select code is modified using the
'' upper 3 bits of the 19 bit addrReg.  This returns true if an error occurred.
'' Note that the long word value may not span an EEPROM page boundary.
   if WRITE_PAGE(devSel, addrReg, @data, 4)
      return true
   ' james edit - wait for 5ms for page write to complete (80_000 * 5 = 400_000)      
   waitcnt(400_000 + cnt)      
   return false

PUB READ_PAGE(devSel, addrReg, dataPtr, count) : ackbit
'' Read in a block of i2c data.  Device select code is devSel.  Device starting
'' address is addrReg.  Data address is at dataPtr.  Number of bytes is count.
'' The device select code is modified using the upper 3 bits of the 19 bit addrReg.
'' Return zero if no errors or the acknowledge bits if an error occurred.
   devSel |= addrReg >> 15 & %1110
   START                          ' Select the device & send address
   ackbit := WRITE(devSel | Xmit)
   ackbit := (ackbit << 1) | WRITE(addrReg >> 8 & $FF)
   ackbit := (ackbit << 1) | WRITE(addrReg & $FF)          
   Start                          ' Reselect the device for reading
   ackbit := (ackbit << 1) | WRITE(devSel | Recv)
   repeat count - 1
      byte[dataPtr++] := READ(ACK)
   byte[dataPtr++] := READ(NAK)
   STOP
   return ackbit

PUB WRITE_PAGE(devSel, addrReg, dataPtr, count) : ackbit
'' Write out a block of i2c data.  Device select code is devSel.  Device starting
'' address is addrReg.  Data address is at dataPtr.  Number of bytes is count.
'' The device select code is modified using the upper 3 bits of the 19 bit addrReg.
'' Most devices have a page size of at least 32 bytes, some as large as 256 bytes.
'' Return zero if no errors or the acknowledge bits if an error occurred.  If
'' more than 31 bytes are transmitted, the sign bit is "sticky" and is the
'' logical "or" of the acknowledge bits of any bytes past the 31st.
   devSel |= addrReg >> 15 & %1110
   START                          ' Select the device & send address
   ackbit := WRITE(devSel | Xmit)
   ackbit := (ackbit << 1) | WRITE(addrReg >> 8 & $FF)
   ackbit := (ackbit << 1) | WRITE(addrReg & $FF)          
   repeat count                        ' Now send the data
      ackbit := ackbit << 1 | ackbit & $80000000 ' "Sticky" sign bit         
      ackbit |= WRITE(byte[dataPtr++])
   STOP
   return ackbit