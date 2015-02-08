{ Radio Module - RFM69 }

CON
  RF_FOSC  = 32_000_000 ' master oscillator
  RF_FSTEP = 61        ' this should be 61.0351 = FOSC / 2^19
                        ' FRF = 434MHz/ Fstep = 7_110_656
  RF_FRF434MSB = $6C      ' FRF[23:16]
  RF_FRF434MID = $80      ' FRF[15:8]
  RF_FRF434LSB = $00      ' FRF[7:0]

CON
' Register names
  RF_REG_00_FIFO        =  $00
  RF_REG_01_OPMODE      =  $01
  RF_REG_02_DATAMODUL   =  $02
  RF_REG_03_BITRATEMSB  =  $03
  RF_REG_04_BITRATELSB  =  $04
  RF_REG_05_FDEVMSB     =  $05
  RF_REG_06_FDEVLSB     =  $06
  RF_REG_07_FRFMSB      =  $07
  RF_REG_08_FRFMID      =  $08
  RF_REG_09_FRFLSB      =  $09
  RF_REG_0A_OSC1        =  $0A
  RF_REG_0B_AFCCTRL     =  $0B
  RF_REG_0C_RESERVED    =  $0C
  RF_REG_0D_LISTEN1     =  $0D
  RF_REG_0E_LISTEN2     =  $0E
  RF_REG_0F_LISTEN3     =  $0F
  RF_REG_10_VERSION     =  $10
  RF_REG_11_PALEVEL     =  $11
  RF_REG_12_PARAMP      =  $12
  RF_REG_13_OCP         =  $13
  RF_REG_14_RESERVED    =  $14
  RF_REG_15_RESERVED    =  $15
  RF_REG_16_RESERVED    =  $16
  RF_REG_17_RESERVED    =  $17
  RF_REG_18_LNA         =  $18
  RF_REG_19_RXBW        =  $19
  RF_REG_1A_AFCBW       =  $1A
  RF_REG_1B_OOKPEAK     =  $1B
  RF_REG_1C_OOKAVG      =  $1C
  RF_REG_1D_OOKFIX      =  $1D
  RF_REG_1E_AFCFEI      =  $1E
  RF_REG_1F_AFCMSB      =  $1F
  RF_REG_20_AFCLSB      =  $20
  RF_REG_21_FEIMSB      =  $21
  RF_REG_22_FEILSB      =  $22
  RF_REG_23_RSSICONFIG  =  $23
  RF_REG_24_RSSIVALUE   =  $24
  RF_REG_25_DIOMAPPING1 =  $25
  RF_REG_26_DIOMAPPING2 =  $26
  RF_REG_27_IRQFLAGS1   =  $27
  RF_REG_28_IRQFLAGS2   =  $28
  RF_REG_29_RSSITHRESH  =  $29
  RF_REG_2A_RXTIMEOUT1  =  $2A
  RF_REG_2B_RXTIMEOUT2  =  $2B
  RF_REG_2C_PREAMBLEMSB =  $2C
  RF_REG_2D_PREAMBLELSB =  $2D
  RF_REG_2E_SYNCCONFIG  =  $2E
  RF_REG_2F_SYNCVALUE1  =  $2F
' another 7 sync word bytes follow, 30 through 36 inclusive
  RF_REG_37_PACKETCONFIG1  =  $37
  RF_REG_38_PAYLOADLENGTH  =  $38
  RF_REG_39_NODEADRS       =  $39
  RF_REG_3A_BROADCASTADRS  =  $3A
  RF_REG_3B_AUTOMODES      =  $3B
  RF_REG_3C_FIFOTHRESH     =  $3C
  RF_REG_3D_PACKETCONFIG2  =  $3D
  RF_REG_3E_AESKEY1        =  $3E
' Another 15 AES key bytes follow
  RF_REG_4E_TEMP1        =    $4E
  RF_REG_4F_TEMP2        =    $4F
  RF_REG_58_TESTLNA      =    $58
  RF_REG_5A_TESTPA1      =    $5A
  RF_REG_5C_TESTPA2      =    $5C
  RF_REG_6F_TESTDAGC     =    $6F
  RF_REG_71_TESTAFC      =    $71

CON
  ' These are the valid settings for the registers

  ' REG_01
  RF_OPMODE_MODE_MASK  = $1C 'bits 4:2
  RF_OPMODE_MODE_SLEEP = $00 ' 000
  RF_OPMODE_MODE_STDBY  = $04 ' 001
  RF_OPMODE_MODE_FS    = $08 ' 010
  RF_OPMODE_MODE_RX    = $0c ' 011
  RF_OPMODE_MODE_TX    = $10 ' 100

  ' REG_25
  RF_DIO0MAPPING_01      = $40 ' bit[7:6] is the setting for DIO0.  01 = signal on packetready
  RF_DIO0MAPPING_00      = $00 ' bit[7:6] is the setting for DIO0.  01 = signal on packetready

  ' REG_26
  RF_CLKOUT_OSCOFF       = $07 ' bit[2:0] control oscillator out.  111 = off
  ' REG_27
  RF_IRQFLAGS1_MODEREADY = $80 ' bit 7

  ' power settings
  ' REG_5A and 5C
  RF_TESTPA1_NORMAL = $55 
  RF_TESTPA1_BOOST = $5D 
  RF_TESTPA2_NORMAL = $70 
  RF_TESTPA2_BOOST = $7C

CON
  RF_MODEIDLE = 0
  RF_MODERX   = 1
  RF_MODETX   = 2
  
CON ' Shared SPI bus pins
  SHARED_MOSI = 22
  SHARED_MISO = 23
  SHARED_SCLK = 21
  WAKEUP      = 14
  RADIO_CS    = 26

OBJ
  UARTS : "FullDuplexSerial4portPlus_0v3"
    
VAR
  ' this is one one idle, rx, or tx
  byte _rfmode, _rfversion, DEBUG
  byte verbose

PUB INIT(debugPort, verboseFlag) : success | ver, rc, nsync, i
  DEBUG := debugPort
  verbose := verboseFlag
  DIRA[RADIO_CS] := 1
  DIRA[SHARED_MOSI] := 1
  DIRA[SHARED_SCLK] := 1

  UARTS.STR(DEBUG, string("RFM: Initializing pins for Radio", 13))

  ' check if the radio is connected and alive
  ver := spiRead(RF_REG_10_VERSION)
  if ver == $00 OR ver == $FF
    if verbose
      UARTS.STR(DEBUG, string("RFM: No radio found", 13))
    success := FALSE
  else
    if verbose
      UARTS.STR(DEBUG, string("RFM: Radio found.  Ver: "))
      UARTS.HEX(DEBUG, ver, 2)
      UARTS.PUTC(DEBUG, 13)
    _rfversion := ver
    success := TRUE

  setModeIdle

  if verbose
    UARTS.STR(DEBUG, string("Dumping all registers", 13))
    repeat i from 1 to $4F
      rc := spiRead(i)
      UARTS.STR(DEBUG, string("Register: "))
      UARTS.HEX(DEBUG, i, 2)
      UARTS.STR(DEBUG, string(" Value: "))
      UARTS.HEX(DEBUG, rc, 2)
      UARTS.PUTC(DEBUG, 13)
    rc := spiRead($58)  
    UARTS.STR(DEBUG, string("Register $58 Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($5A)  
    UARTS.STR(DEBUG, string("Register $5A Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)
    
    rc := spiRead($5C)  
    UARTS.STR(DEBUG, string("Register $5C Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($6F)  
    UARTS.STR(DEBUG, string("Register $6F Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($71)
    UARTS.STR(DEBUG, string("Register $71 Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

  ' configure FIFO
  ' bit 7 - start tx as soon as data in fifo
  ' bit6:0 - fifo thresh set to  15
  spiWrite(RF_REG_3C_FIFOTHRESH, $80 | $0F)

  ' configure Digital AGC
  ' this is for "low beta = 0"
  spiWrite(RF_REG_6F_TESTDAGC, $30)

  ' set the sync words AA and 55
  nsync := 2
  spiWrite (RF_REG_2F_SYNCVALUE1, $AA)
  repeat
    if verbose
      UARTS.STR(DEBUG, string("RFM: Waiting for sync1", 13))
  until (spiRead(RF_REG_2F_SYNCVALUE1) == $AA)

  spiWrite (RF_REG_2F_SYNCVALUE1 + 1, $55)
  repeat
    if verbose
      UARTS.STR(DEBUG, string("RFM: Waiting for sync2", 13))
  until (spiRead(RF_REG_2F_SYNCVALUE1 + 1) == $55)

  rc := spiRead(RF_REG_2E_SYNCCONFIG)
  rc |= (nsync-1) << 3 ' set the syncsize bits (to one less)
  spiWrite(RF_REG_2E_SYNCCONFIG, rc)

  ' configure modulation to GFSK (options are FSK, OOK)
  ' packetmode_fsk_unused_gaussian1.0
  spiWrite(RF_REG_02_DATAMODUL, %00_00_0_01)
  ' set datarate = Fosc/4800 = 6667
  spiWrite(RF_REG_03_BITRATEMSB, $1A)
  spiWrite(RF_REG_04_BITRATELSB, $0B)
  ' set the Freq dev = 9.6khz = 61*157 = Fstep * Fdev[15:0]
  spiWrite(RF_REG_05_FDEVMSB, $00)
  spiWrite(RF_REG_06_FDEVLSB, $9D)

  ' set the RX BW = Fosc / bwmant * 2 ^ (bwexp+2) = 10.4Khz, which is 2x the bitrate of 4.8Khz
  ' leave at defaults DCC = 2, BWMANT = 01b, which is 24
  ' and BWEXP = 5

  ' set the packet format
  ' variable length_dcwhitening_crcon_crcautoclear_addressingoff
  spiWrite(RF_REG_37_PACKETCONFIG1, %1_10_1_0_00)
  rc := spiRead(RF_REG_37_PACKETCONFIG1)
  spiWrite(RF_REG_3D_PACKETCONFIG2, rc & !1) ' clear the encrypt bit - no encrypt

  ' set the DIO mapping
  spiWrite(RF_REG_25_DIOMAPPING1, RF_DIO0MAPPING_01)
  spiWrite(RF_REG_26_DIOMAPPING2, RF_CLKOUT_OSCOFF)
  
  ' set the transmit frequency
  spiWrite(RF_REG_07_FRFMSB, RF_FRF434MSB)
  spiWrite(RF_REG_08_FRFMID, RF_FRF434MID)
  spiWrite(RF_REG_09_FRFLSB, RF_FRF434LSB)

  if verbose
    UARTS.STR(DEBUG, string("Dumping all registers", 13))
    repeat i from 1 to $4F
      rc := spiRead(i)
      UARTS.STR(DEBUG, string("Register: "))
      UARTS.HEX(DEBUG, i, 2)
      UARTS.STR(DEBUG, string(" Value: "))
      UARTS.HEX(DEBUG, rc, 2)
      UARTS.PUTC(DEBUG, 13)
    rc := spiRead($58)  
    UARTS.STR(DEBUG, string("Register $58 Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($5A)  
    UARTS.STR(DEBUG, string("Register $5A Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)
    
    rc := spiRead($5C)  
    UARTS.STR(DEBUG, string("Register $5C Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($6F)  
    UARTS.STR(DEBUG, string("Register $6F Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)

    rc := spiRead($71)
    UARTS.STR(DEBUG, string("Register $571 Value: "))
    UARTS.HEX(DEBUG, rc, 2)
    UARTS.PUTC(DEBUG, 13)
 
  return

  
'  repeat
'    OUTA[RADIO_CS] := 0
'    PAUSE_MS(100)       
'    OUTA[RADIO_CS] := 1
'    PAUSE_MS(100)
            ' 
PUB setModeIdle
  if verbose
    UARTS.STR(DEBUG, string("RFM: Setting mode to idle", 13))
    
  if _rfmode <> RF_MODEIDLE
    spiWrite(RF_REG_5A_TESTPA1, RF_TESTPA1_NORMAL)  
    spiWrite(RF_REG_5C_TESTPA2, RF_TESTPA2_NORMAL)

  setOpMode(RF_OPMODE_MODE_STDBY)
  _rfmode := RF_MODEIDLE

PUB sendHello | b
  ' set DIO
  if _rfmode <> RF_MODEIDLE
    setModeIdle
    
  spiWrite(RF_REG_25_DIOMAPPING1, RF_DIO0MAPPING_00) ' dio0 will indicate packetsent

  ' write a packet, preceded by length
  OUTA[RADIO_CS] := 0
  b := RF_REG_00_FIFO | $80  ' fifo register write
  sendByte(b)
  ' length (1+2) and packet (Hi)
  b := 3
  sendByte(b)
  b := "H"
  sendByte(b)
  b := "i"
  sendByte(b)
  OUTA[RADIO_CS] := 1
  setModeTX
  ' FIXME wait for DIO0 to rise, or timeout
  waitcnt(clkfreq + cnt) ' wait 1s
  setModeIdle

PUB readFIFO | b
  if _rfmode <> RF_MODEIDLE
    setModeIdle
    
  spiWrite(RF_REG_25_DIOMAPPING1, RF_DIO0MAPPING_01) ' dio0 will indicate packetready

  ' read a packet, preceded by length
  OUTA[RADIO_CS] := 0
  b := RF_REG_00_FIFO  ' fifo register read
  b:=getByte
  UARTS.STR(DEBUG, string("RFM: FIFO: "))
  UARTS.DEC(DEBUG, b)
  UARTS.PUTC(DEBUG,44)
    
  b := getByte
  UARTS.DEC(DEBUG, b)
  UARTS.PUTC(DEBUG,44)

  b := getByte
  UARTS.DEC(DEBUG, b)
  UARTS.PUTC(DEBUG,44)

  OUTA[RADIO_CS] := 1
  'setModeTX
  ' FIXME wait for DIO0 to rise, or timeout
  'waitcnt(clkfreq + cnt) ' wait 1s
  'setModeIdle

PUB readRSSI : rssi
  rssi := spiRead(RF_REG_24_RSSIVALUE)
  rssi >>= 1
    
PUB setModeTX
  if verbose
    UARTS.STR(DEBUG, string("RFM: Setting mode to TX", 13))

  ' set to high power  
  spiWrite(RF_REG_5A_TESTPA1, RF_TESTPA1_BOOST)  
  spiWrite(RF_REG_5C_TESTPA2, RF_TESTPA2_BOOST)
  
  setOpMode(RF_OPMODE_MODE_TX)
  _rfmode := RF_MODETX
 
PUB setModeRX
  if verbose
    UARTS.STR(DEBUG, string("RFM: Setting mode to RX", 13))

  ' set to normal power  
  spiWrite(RF_REG_5A_TESTPA1, RF_TESTPA1_NORMAL)  
  spiWrite(RF_REG_5C_TESTPA2, RF_TESTPA2_NORMAL)

  spiWrite(RF_REG_25_DIOMAPPING1, RF_DIO0MAPPING_01) ' dio0 will indicate packetready in rx mode

  setOpMode(RF_OPMODE_MODE_RX)
  _rfmode := RF_MODERX

PUB setOpMode(regmode) | regVal
  regVal := spiRead(RF_REG_01_OPMODE)
  if verbose
    UARTS.STR(DEBUG, string("RFM: Current OPMODE: "))
    UARTS.HEX(DEBUG, regVal, 2)
    UARTS.PUTC(DEBUG, 13)
    UARTS.STR(DEBUG, string("RFM: Setting OPMODE: "))
    UARTS.HEX(DEBUG, regmode, 2)
    UARTS.PUTC(DEBUG, 13)
    
  regVal &= !(RF_OPMODE_MODE_MASK) ' clear the mode bits
  
  regVal |= (regmode & RF_OPMODE_MODE_MASK) ' and set the mode bits
  spiWrite(RF_REG_01_OPMODE, regVal)

  repeat
    if verbose
      UARTS.STR(DEBUG, string("RFM: Waiting for mode change", 13))
  until (spiRead(RF_REG_27_IRQFLAGS1) & RF_IRQFLAGS1_MODEREADY)

PUB spiRead(addr) : response
  ' read from register at addr
  ' rf69 require bit 7 as w/!r
  ' so if bit7 is 1, return with error ($ff?) - fixme

  if addr > $7f
    return $ff
    
  'UARTS.HEX(DEBUG, addr, 2)
  'UARTS.PUTC(DEBUG, 13)

  addr ><= 8 ' reverse order so MSB is at lsb
  'UARTS.HEX(DEBUG, addr, 2)
  'UARTS.PUTC(DEBUG, 13)
  
  OUTA[RADIO_CS] := 1
  OUTA[SHARED_SCLK] := 0

  OUTA[RADIO_CS] := 0 ' assert cs
  ' place a bit on the mosi line so it is valid on the rising edge of sclk
  repeat 8
    OUTA[SHARED_MOSI] := (addr & 1)
    OUTA[SHARED_SCLK] := 1
    OUTA[SHARED_SCLK] := 0
    addr >>= 1

  OUTA[SHARED_MOSI] := 0

  response := 0
  repeat 8
    OUTA[SHARED_SCLK] := 1
    response := (response << 1) + INA[SHARED_MISO]
    OUTA[SHARED_SCLK] := 0

  OUTA[RADIO_CS] := 1
  return response  

PUB spiWrite(addr, value)
  ' write value to addr
  ' rfm69 chip uses bit 7 of address as w/!r
  ' set to 1 to write value   

  'UARTS.HEX(DEBUG, addr, 2)
  'UARTS.PUTC(DEBUG, 13)
  addr |= (1<<7) ' bit7 =1 means write to addr
  addr ><= 8 ' reverse order so MSB is at lsb
  'UARTS.HEX(DEBUG, addr, 2)
  'UARTS.PUTC(DEBUG, 13)
  
  OUTA[RADIO_CS] := 1
  OUTA[SHARED_SCLK] := 0

  OUTA[RADIO_CS] := 0 ' assert cs
  ' place a bit on the mosi line so it is valid on the rising edge of sclk
  repeat 8
    OUTA[SHARED_MOSI] := (addr & 1)
    OUTA[SHARED_SCLK] := 1
    OUTA[SHARED_SCLK] := 0
    addr >>= 1

  OUTA[SHARED_MOSI] := 0

  value ><= 8
  repeat 8
    OUTA[SHARED_MOSI] := (value & 1)
    OUTA[SHARED_SCLK] := 1
    OUTA[SHARED_SCLK] := 0
    value >>= 1

  OUTA[RADIO_CS] := 1
  return

PRI sendByte(b)
  repeat 8
    OUTA[SHARED_MOSI] := (b & 1)
    OUTA[SHARED_SCLK] := 1
    OUTA[SHARED_SCLK] := 0
    b >>= 1

  OUTA[SHARED_MOSI] := 0

PRI getByte : response
  response := 0
  repeat 8
    OUTA[SHARED_SCLK] := 1
    response := (response << 1) + INA[SHARED_MISO]
    OUTA[SHARED_SCLK] := 0
       
       