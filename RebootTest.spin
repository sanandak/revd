'**************************************************************
'* RebootTest.spin
'* quick test of the reboot feature of the prop.
'*
'**************************************************************
CON ' Clock mode settings
  _CLKMODE = XTAL1 + PLL16X
  _XINFREQ = 5_000_000


OBJ                                  
  PST     : "Parallax Serial Terminal"       

PUB MAIN | idx


  PST.START(115200)
  PAUSE_MS(1_000)
  PST.STR(string(13, "Testing reboot command."))
  
  repeat
    PAUSE_MS(1_000)
    PST.STR( string(13, "Just waiting around: "))
    PST.DEC( idx++)
    if idx == 12
      PST.STR( string(13, "Time for reboot."))
      PAUSE_MS(1000)
      reboot
     
                                         
 
PUB PAUSE_MS(mS)
  waitcnt(clkfreq/1000 * mS + cnt)

