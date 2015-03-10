{{
UbloxMessages.spin


Use this program to turn on the messages we want.

PGB
7 Dec 2013
McMurdo


}}

CON ' System Clock Settings

  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 6_250_000

  CLK_FREQ = ((_clkmode - xtal1) >> 6) * _xinfreq               ' system freq as a constant
  ONE_MS   = CLK_FREQ / 1_000                                   ' ticks in 1ms
  ONE_US   = CLK_FREQ / 1_000_000                               ' ticks in 1us
 
CON
  LOW               =  0
  HIGH              =  1
  INPUT             =  0
  OUTPUT            =  1

CON ' I2C expander
  I2C_SDA = 29                  ' I2C bus data
  I2C_SCL = 28                  ' I2C bus clock

  PCF8574 = %0111_000_0         ' I2C expander address as per the datasheet bits 3-1 are set by layout
  EXPANDER_ADDRESS = %111       ' the address for the I2C expander as per our design
  WRITE_BIT = 0                   ' for write operation R/W bit should be LOW
  READ_BIT  = 1                   ' for read operation R/W bit should be HIGH

  #0, ACK, NAK

CON ' GPS constants including bit on I2C expander
  GPS_RESET         = 4         ' this bit is set/reserved on the I2C expander

  GPS_RX_FROM       = 2
  GPS_TX_TO         = 3
  GPS_PPS           = 1

CON ' Constants for processing the ublox data
  ' state machine
  #0, WAIT_FOR_HEADER1, WAIT_FOR_HEADER2, WAIT_FOR_CLASS, WAIT_FOR_MSGID, WAIT_FOR_LEN0, WAIT_FOR_LEN1, READ_PAYLOAD, READ_CKSUM0, READ_CKSUM1, DONE
  ' pkt numbers
  #1, NAVPOSLLH, NAVSTATUS, NAVTIMEGPS, NAVTIMEUTC, NAVSVINFO, RXMRAW, ACKACK, ACKNAK, MONVER, LASTPKT

  POSLLH_LEN  = 28 'payload len
  STATUS_LEN  = 16
  TIMEGPS_LEN = 16
  TIMEUTC_LEN = 20
  SVINFO_LEN  = 400
  RAW_LEN     = 400
  ACK_LEN     = 2
  NAK_LEN     = 2
  MONVER_LEN  = 70

CON
  TEST_PT     = 27

CON  '' Serial port pins and baud settings
  TAB               = 09
  LF                = 10                                  ' ASCII <LF>
  CR                = 13                                  ' ASCII <CR>
  SPACE             = 32
  DASH              = 45
  DOT               = 46
  COLON             = 58

OBJ
'******************************************* Object to Include *************************************
  uarts    : "FullDuplexSerial4portPlus_0v3"       ' 1 COG for 3 serial ports modified by Tracy Allen
  Num      : "Numbers"                             ' Include Numbers object for writing numbers to debuginal


VAR
'******************************************* Variable declarations *********************************
  byte debug, gps_port
  byte state
  byte expanderValue
  byte timeutcPkt[TIMEUTC_LEN]
  byte posllhPkt[POSLLH_LEN]
  byte statusPkt[STATUS_LEN]
  byte timegpsPkt[TIMEGPS_LEN]
  byte rawPkt[RAW_LEN]
  byte svinfoPkt[SVINFO_LEN]
  byte cksum[2], ckA, ckB
  byte pktType
  
  byte ackPkt[ACK_LEN]
  byte nakPkt[NAK_LEN]
  byte monverPkt[MONVER_LEN]

  byte ubxBuffer[1024]
  'byte tempBuffer[1024]
  
  byte nPktsPerSec                 ' keep track of number of packets in this sec
  byte class, msgid, pkt
  byte statusFix                      ' 0 = no fix, 2 = 2d, 3 = 3d
  ' timegps
  byte leapS
  byte gpsValid
  byte utcmonth, utcday, utchour, utcmin, utcsec, utcValid
  ' svinfo
  byte nSV, svs[16] 'FIXME

  byte swVersion[30]
  byte hwVersion[30]
  byte capturePPS  

  word ubxBufferIdx1, ubxBufferIdx2, payloadIdx, len
  word utcyear

  long accuracy
  long cntsAtPPS 
  long expanderId     ' set up a register that contains the complete address for the I2C expander
  long cntsAtHeader1
  
  long TOWmS                         ' ms from start of week
  ' posllh
  long longitude, latitude, height ' degrees scaled by 1e-7 and mm
  ' status

DAT ' GPS Config messages
  TIME_PULSE byte 
'******************************************* Public and Private Methods ****************************
PUB INIT(_debug, _gps) | idx 

  debug    := _debug
  gps_port := _gps
  state    := WAIT_FOR_HEADER1

  CLEAR_UBX_BUFFER
  payloadIdx    := 0
'  repeat idx from 0 to 1023
'    tempBuffer[idx] := idx

  return @ubxBuffer
  
PUB VERSION_POLL | nTry, idx, _pkt
  'query u-blox for sw/hw version 
  ' return -1 on failure, 0 on success (?)

  bytefill(@swVersion, 0, 30)
  bytefill(@hwVersion, 0, 10)
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_MON_VER, 8)
'  repeat idx from 0 to 7
'    uarts.hex(debug, CFG_MSG_MON_VER[idx], 2)
'    uarts.putc(debug, " ")

  state := WAIT_FOR_HEADER1
  nTry := 0
  repeat
    repeat idx from 0 to 7
      uarts.putc(gps_port, CFG_MSG_MON_VER[idx])

    idx := 0 ' now this counts number of bytes
    state := WAIT_FOR_HEADER1
  
    repeat
      _pkt := READ_AND_PROCESS_BYTE(TRUE)
    until (_pkt == MONVER) OR (idx++ == 500)
  until (_pkt == MONVER) OR nTry++ == 5
  if nTry >= 5 
    uarts.str(debug, string(13, "After version-query loop, nTry = "))
    uarts.dec(debug, nTry)
    return -1
{{
  uarts.putc(debug, ">")
  uarts.str(debug, @swVersion)
  uarts.putc(debug, "<")
  uarts.putc(debug, 13)
  uarts.putc(debug, ">")
  uarts.str(debug, @hwVersion)
  uarts.putc(debug, "<")
  uarts.putc(debug, 13)
}}
  return 0

PUB SWADDR
  return @swVersion
PUB HWADDR
  return @hwVersion

PUB TURN_OFF_NMEA | _pkt, nTry, idx
  UARTS.STR(DEBUG, string(13, "$PSMSG, GPS NMEA OFF"))
  ' turn off gll
  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $01
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off GLL"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off GLL nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $00
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off GGA"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off GGA nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $02
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off GSA"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off GSA nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $03
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off GSV"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off GSV nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $08
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off ZDA"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off ZDA nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $04
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off RMC"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off RMC nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  CFG_MSG_NMEA[6] := $F0
  CFG_MSG_NMEA[7] := $05
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG_NMEA, 16)
'  uarts.str(debug, string(13, "Turn off VTG"))
'  uarts.putc(debug, 13)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG_NMEA[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG_NMEA, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn off VTG nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

PUB TURN_ON_RAW | _pkt, nTry, idx
  nTry:=0
  UARTS.STR(DEBUG, string(13, "$PSMSG, GPS RAW ON"))
  ' turn on POSLLH
  CFG_MSG[6] := $01
  CFG_MSG[7] := $02
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON POSLLH nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  ' turn on TIMEGPS
  CFG_MSG[6] := $01
  CFG_MSG[7] := $20
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON TIMEGPS nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  ' turn on TIMEUTC
  CFG_MSG[6] := $01
  CFG_MSG[7] := $21
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON TIMEUTC nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  ' turn on STATUS
  CFG_MSG[6] := $01
  CFG_MSG[7] := $03
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON STATUS nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  ' turn on SVINFO
  CFG_MSG[6] := $01
  CFG_MSG[7] := $30
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON SVINFO nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

  ' turn on RAW
  CFG_MSG[6] := $02
  CFG_MSG[7] := $10
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
'  repeat idx from 0 to 15
'    uarts.hex(debug, CFG_MSG[idx], 2)
'    uarts.putc(debug, " ")
  nTry := 0
  repeat 
    TXMSG(@CFG_MSG, 16)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)
'  uarts.str(debug, string(13, "After Turn ON RAW nTry, ack/nak = "))
'  uarts.dec(debug, nTry)
'  uarts.putc(debug, " ")
'  uarts.dec(debug, _pkt)
'  uarts.putc(debug, 13)

PUB TURN_ON_PPS | _pkt, nTry, idx
  UARTS.STR(DEBUG, string(13, "$PSMSG, 1PPS ON"))
  ' set up time pulses
'  repeat idx from 0 to 39
'    uarts.hex(debug, CFG_TP5[idx], 2)
'    uarts.putc(debug, " ")
  repeat 
    TXMSG(@CFG_TP5, 40)
    _pkt := GETACK
  until _pkt == ACKACK OR (nTry++ == 5)

PRI TXMSG(msgAddr, msgLen) | idx
  repeat idx from 0 to (msgLen-1)       ' send the bytes out the door
    uarts.putc(gps_port, byte[msgAddr][idx])

PRI GETACK : _pkt | idx
  ' look for ack or nak packet in next 500 bytes
  idx := 0 ' now this counts number of bytes
  state := WAIT_FOR_HEADER1
  
  repeat
    _pkt := READ_AND_PROCESS_BYTE(TRUE)
'      uarts.hex(debug, _pkt, 2)
'      uarts.putc(debug, SPACE)
'      PAUSE_MS(5)
  until (_pkt == ACKACK) OR (_pkt == ACKNAK) OR (idx++ == 500)
  

PUB SEND_CFG_MSG(cfgclass, cfgmsgid, rate) | _pkt, idx, nTry, tStart
'' send configure message to the UBLOX
  CFG_MSG[6] := cfgclass
  CFG_MSG[7] := cfgmsgid
  CFG_MSG[9] := rate
  CALCULATE_RAW_UBLOX_CKSUM(@CFG_MSG, 16)
  nTry := 0

'  uarts.str(debug, string(13,"_pkt: "))
  repeat
    repeat idx from 0 to 15       ' send the bytes out the door
      uarts.putc(gps_port, CFG_MSG[idx])
    _pkt := -1
    idx := 0
    repeat
      tStart := cnt
      repeat 
        _pkt := READ_AND_PROCESS_BYTE(TRUE)
        'uarts.dec(debug, _pkt)  
        PAUSE_MS(1)   ' don't go crazy this things is slow so give it a chance.  
      until (_pkt == ACKACK) OR (_pkt == ACKNAK) OR (idx++ == 10)
    until (cnt - tStart ) > CONSTANT(5*ONE_MS)
  until _pkt == ACKACK OR nTry++ == 5
  
PUB SOFT_RESET | idx
  repeat 12
    uarts.putc(gps_port, CFG_RST[idx++])
  
PUB READ_AND_PROCESS_BYTE(blocking) : retVal | rxByte 
{{
Take byte from main cog and add it to gps buffer and process messages if appropriate.
(2 bytes), packet class & id,
length (2 bytes), payload (len bytes), and cksum (2 bytes)

Parameters:
  pktIdx - passed by calling process, index into ubxBuffer to store data
           Set to zero after pps 
  doTimeout - Boolean to request timeout if no bytes received for 0.1s (end of packets)
              FIXME - should I use pktIdx

Return:
  retVal: -1 - no byte received
           0 - byte received and processed
          >1 - pktType see below

Actions
  FIXME use the same storage location for all payloads
  
}}

  retVal  := 0

' sak changed to be blocking if blocking param is set
  if blocking
    if state == WAIT_FOR_HEADER1 ' ignore all bytes except hdr1 (for example NMEA cruft)
      repeat         
        rxByte := uarts.rx(gps_port)    ' collect byte from GPS port    
      until rxByte == $B5 
    else ' OK, now process all bytes...
      rxByte := uarts.rx(gps_port)
  else 
    rxByte := uarts.rxcheck(gps_port)    ' collect byte from GPS port                  
    if rxByte == -1                      ' -1 means no data; don't do anything
      return retVal := -1

  ubxBuffer[ubxBufferIdx1-ubxBufferIdx2] := rxByte
  ubxBufferIdx2++
  if ubxBufferIdx2 == 4
    ubxBufferIdx1 += 4
    ubxBufferIdx2 := 0
      
  if ubxBufferIdx1 > 1024
     ubxBufferIdx1 := 3                 ' limit max val for ubxBufferIdx1 to avoid memory overruns.        
     ubxBufferIdx2 := 0

  case state
    WAIT_FOR_HEADER1 :
      if rxByte == $B5
        state := WAIT_FOR_HEADER2

    WAIT_FOR_HEADER2 :
      if (rxByte == $62) ' byte should be header2... ' AND ((idx-header1idx) == 1)
        state := WAIT_FOR_CLASS
      else
        state := WAIT_FOR_HEADER1 ' if not...return to header1 otherwise

    WAIT_FOR_CLASS :
      ckA := 0
      ckB := 0
      class := rxByte
      ckA := ckA + rxByte
      ckB := ckB + ckA
      state := WAIT_FOR_MSGID


    WAIT_FOR_MSGID :
      msgid := rxByte
      ckA := ckA + rxByte
      ckB := ckB + ckA
      state := WAIT_FOR_LEN0
     case ((class << 8) & $FF00) | msgid
        $01_02 :
          pktType := NAVPOSLLH
        $01_03 :
          pktType := NAVSTATUS
        $01_20 :
          pktType := NAVTIMEGPS
        $01_21 :
          pktType := NAVTIMEUTC
        $01_30 :
          pktType := NAVSVINFO
        $02_10 :
          pktType := RXMRAW
        $05_01 :
          pktType := ACKACK
        $05_00 :
          pktType := ACKNAK
        $0A_04 :
          pktType := MONVER
        OTHER :
          pktType := -1
          state   := WAIT_FOR_HEADER1

    WAIT_FOR_LEN0 :
      ckA := ckA + rxByte
      ckB := ckB + ckA
      len   := rxByte
      state := WAIT_FOR_LEN1

    WAIT_FOR_LEN1 :
      len |= ((rxByte << 8) & $FF00)
      payloadIdx := 0
      ckA := ckA + rxByte
      ckB := ckB + ckA
      state := READ_PAYLOAD

    READ_PAYLOAD :        
      ckA := ckA + rxByte
      ckB := ckB + ckA
      case pktType
        NAVPOSLLH : 
          posllhPkt[payloadIdx++]  := rxByte
        NAVSTATUS :
          statusPkt[payloadIdx++]  := rxByte
        NAVTIMEGPS :
          timegpsPkt[payloadIdx++] := rxByte
        NAVTIMEUTC :
          timeutcPkt[payloadIdx++] := rxByte
        NAVSVINFO :
          svinfoPkt[payloadIdx++]  := rxByte
        RXMRAW :
          rawPkt[payloadIdx++]     := rxByte
        ACKACK :
          ackPkt[payloadIdx++]     := rxByte
        ACKNAK :
          nakPkt[payloadIdx++]     := rxByte
        MONVER :
          monverPkt[payloadIdx++]  := rxByte
        OTHER:
          state := WAIT_FOR_HEADER1
      if payloadIdx == len
        state := READ_CKSUM0
        
    READ_CKSUM0 :
      state := READ_CKSUM1
      cksum[0] := rxByte

    READ_CKSUM1 :
      state      := WAIT_FOR_HEADER1
      cksum[1]   := rxByte
      'bytemove(@ubxBuffer, @tempBuffer, 1024)
 
      if (cksum[0] == (ckA & $FF)) AND (cksum[1] == (ckB & $FF))
        retVal := PARSE_UBLOX_PACKET
  
  return retVal

PUB PARSE_UBLOX_PACKET :retVal | i
'' we know what packet just arrived so let's process it

  case pktType
    NAVPOSLLH :
'      TOWmS     := SWAP_BYTES_LONG(@posllhPkt, 0)
      longitude := SWAP_BYTES_LONG(@posllhPkt, 4)
      latitude  := SWAP_BYTES_LONG(@posllhPkt, 8)
      height    := SWAP_BYTES_LONG(@posllhPkt, 12)

    NAVSTATUS :
'      TOWmS     := SWAP_BYTES_LONG(@statusPkt, 0)
      statusFix := statusPkt[4] 

    NAVTIMEGPS :
'      TOWmS     := SWAP_BYTES_LONG(@timegpsPkt, 0)
      leapS     := timegpsPkt[10]
      gpsValid  := timegpsPkt[11]
      accuracy  := timegpsPkt[12] |  timegpsPkt[13] << 8 | timegpsPkt[14] << 16 | timegpsPkt[15] << 24 

    NAVTIMEUTC :
      TOWmS     := SWAP_BYTES_LONG(@timeutcPkt, 0)
      utcyear   := SWAP_BYTES_WORD(@timeutcPkt, 12) 'timeutcPkt[12] | timeutcPkt[13] << 8 ' bytes 12-13
      utcmonth  := timeutcPkt[14]
      utcday    := timeutcPkt[15]
      utchour   := timeutcPkt[16]
      utcmin    := timeutcPkt[17]
      utcsec    := timeutcPkt[18]
      utcValid  := timegpsPkt[19] & %111 ' validUTC | validWKN | validTOW

    NAVSVINFO :
'      TOWmS     := SWAP_BYTES_LONG(@svinfoPkt, 0)
      nSV       := 0
      repeat i from 0 to 15   ' FIXME
        if svinfoPkt[10 + 12 * i] & $01        ' 8 bytes of fixed, then repeat 16 times...
          svs[nSV++] := svinfoPkt[9 + 12 * i]

    MONVER :
      bytemove(@swVersion, @monverPkt, 30)
      bytemove(@hwVersion, @monverPkt[30], 10)

    RXMRAW    : ' scoping these messages suggests that this is the final message in the list
                ' so when we've received all the bytes for this message/packet, we are done
                ' for this second.  
                
  retVal := pktType

PUB BUFFER_ADDRESS
  return @ubxBuffer
  
PUB CLEAR_UBX_BUFFER
  bytefill(@ubxBuffer, 0, 1024)
  ubxBufferIdx1 := 3            ' first byte goes into slot 3
  ubxBufferIdx2 := 0            ' we need to walk backwards through buffer so use this to keep track of subtraction

PUB CALCULATE_RAW_UBLOX_CKSUM(messageAddress, length) | idx 
' calculate the cksum which does not include the sync characters at position 0 and 1
' and does not include the final two bytes of the message because that's where ckA and
' ckB end up.  So we go from message[2] through message[length - 2]

  ckA := 0
  ckB := 0

  ' perform the calculation
  repeat idx from 2 to length - 1 - 2 ' start 2 position and end 2 slots shy of length  
    ckA := ckA + byte[messageAddress][idx]
    ckB := ckB + ckA

  ' place cksum values into final spots in message 
  byte[messageAddress][length-2] := ckA
  byte[messageAddress][length-1] := ckB
    
PUB SWAP_BYTES_LONG(lA, ofs)
  'uarts.putc(debug, byte[lA][ofs])
  'uarts.putc(debug, SPACE)
  return byte[lA][ofs] | byte[lA][ofs+1] << 8 | byte[lA][ofs+2] << 16 | byte[lA][ofs+3] << 24

PUB SWAP_BYTES_WORD(wA, ofs)  
  return byte[wA][ofs] | byte[wA][ofs+1] << 8

PUB GPS_TIME_VALID ' Valid_leap_seconds | valid_week_number | valid_time_of_week
  return gpsValid
  
PUB UTC_TIME_VALID
  return utcValid
'  if (utcValid & %0100) == %0100
'    return TRUE
'  else
'    return FALSE
    
PUB GET_PKT
  return pkt

PUB GET_STATUS_FIX
  return statusFix

PUB GET_LONGITUDE
  return longitude

PUB GET_LATITUDE
  return latitude

PUB GET_ELEVATION
  return height

PUB YEAR
  return utcyear

PUB MONTH
  return utcmonth

PUB DAY
  return utcday

PUB HOUR
  return utchour

PUB MINUTE
  return utcmin

PUB SECOND
  return utcsec  

PUB YEAR_MONTH_DAY
  return (utcyear * 10_000 + utcmonth * 100 + utcday)

PUB HOUR_MINUTE_SECOND
  return (utchour * 10_000 + utcmin * 100 + utcsec)

PUB ACCURACY_NS
  return accuracy

PUB NUM_SVs
  return nSV
  
PUB UPDATE_STATUS_FIX_LINES
'  statusFixStr          byte "Fix:  0 NSV:  00", 0
  statusFixStr[6]  := statusFix + "0"
  statusFixStr[14] := nSV /  10 + "0"
  statusFixStr[15] := nSV // 10 + "0"
  return @statusFixStr
  
PUB UPDATE_LAT_LINES | d, m, s
' uarts.dec(debug, latitude)
' uarts.putc(debug, CR)
' uarts.dec(debug, longitude)
' uarts.putc(debug, CR)
  d := ||latitude / 10_000_000
  m := (||latitude - d * 10_000_000) * 60 / 10_000_000
  s := (||latitude - (d * 10_000_000 + m * 10_000_000 / 60)) * 3600 / 10_000_000  
  if latitude < 0
    gpsLatStr[4] := "S"
  bytemove(@gpsLatStr[6], Num.ToStr(d, Num#DEC3), 3)
  bytemove(@gpsLatStr[9], Num.ToStr(m, Num#DEC3), 3)
  bytemove(@gpsLatStr[12], Num.ToStr(s, Num#DEC3), 3)
  gpsLatStr[9]  := " "
  gpsLatStr[12] := 39  ' set to '
  gpsLatStr[15] := 34  ' set to "
  return @gpsLatStr
  
PUB UPDATE_LON_LINES | d, m, s
  d := ||longitude / 10_000_000
  m := (||longitude - d * 10_000_000) * 60 / 10_000_000
  s := (||longitude - (d * 10_000_000 + m * 10_000_000 / 60)) * 3600 / 10_000_000 
  if longitude < 0
    gpsLonStr[6] := "W"
  bytemove(@gpsLonStr[5], Num.ToStr(d, Num#DEC4), 4)
  bytemove(@gpsLonStr[9], Num.ToStr(m, Num#DEC3), 3)
  bytemove(@gpsLonStr[12], Num.ToStr(s, Num#DEC3), 3)
  gpsLonStr[9]  := " "
  gpsLonStr[12] := 39
  gpsLonStr[15] := 34
  return @gpsLonStr

PUB UPDATE_DATE_TIME_LINES
  gpsTimeDateStr[0] := (utcyear//100)/10 + "0"
  gpsTimeDateStr[1] :=  utcyear//10      + "0"
  gpsTimeDateStr[2] :=  utcmonth/10      + "0"
  gpsTimeDateStr[3] :=  utcmonth//10     + "0"
  gpsTimeDateStr[4] :=  utcday/10        + "0"
  gpsTimeDateStr[5] :=  utcday//10       + "0"
  gpsTimeDateStr[6] := " "
  gpsTimeDateStr[7] := " "
  gpsTimeDateStr[8] :=  utchour/10       + "0"
  gpsTimeDateStr[9] :=  utchour//10      + "0"
  gpsTimeDateStr[10] := ":"
  gpsTimeDateStr[11] := utcmin/10        + "0"
  gpsTimeDateStr[12] := utcmin//10       + "0"
  gpsTimeDateStr[13] := ":"
  gpsTimeDateStr[14] := utcsec/10        + "0"
  gpsTimeDateStr[15] := utcsec//10       + "0"
  return @gpsTimeDateStr
  
PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

PUB PAUSE_US(uS)
  waitcnt(clkfreq/1_000_000 * uS + cnt)

DAT 'CFG_MSG
  CFG_MSG             byte $B5, $62, $06, $01, $08, $00, $FF, $FF, $00, $01, $00, $00, $00, $00, $00, $00

DAT 'CFG_MSG_POLL
'  CFG_MSG_POLL        byte $B5, $62, $06, $01, $02, $00, $FF, $FF, $00, $00
                                                        'clss msg       uartrate
DAT 'CFG_TP5
  CFG_TP5             byte $B5, $62, $06, $31, $20, $00, $00, $01, $00, $00, $00, $00, $00, $00, $01, $00
                      byte $00, $00, $01, $00, $00, $00, $A0, $86, $01, $00, $00, $00, $00, $00, $00, $00
                      byte $00, $00, $7B, $08, $00, $00, $04, $02
                           
DAT 'CFG_MSG_MON_VER
  CFG_MSG_MON_VER     byte $B5, $62, $0A, $04, $00, $00, $00, $00

DAT 'CFG_RST
  CFG_RST             byte $B5, $62, $06, $04, $04, $00, $00, $00, $02, $00, $10, $68

DAT 'CFG_MSG_NMEA
  CFG_MSG_NMEA        byte $B5, $62, $06, $01, $08, $00, $F0, $01, $00, $00, $00, $00, $00, $01, $00, $00

DAT 'String constants
                           '0000000000111111
                           '0123456789012345
  statusFixStr       byte "Fix:  0 NSV:  00", 0         
  gpsLatStr          byte "LAT S  80D30M30S", 0
  gpsLonStr          byte "LON W 120D30M30S ", 0
  gpsDateStr         byte "YYYY-MM-DD      ", 0
  gpsTimeStr         byte "HH:MM:SS        ", 0
  gpsTimeDateStr     byte "YYMMDD  HH:MM:SS", 0
  
               