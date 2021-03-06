''*****************************
''*  Sparecogs by Alex        *
''*****************************

VAR

  byte  cog[8]


PUB freecount : count | i, c
''counts the free cogs returns the count and fills in a string to show which cogs are free and in use
  i :=0
  repeat 'loads as many cogs as possible and stores their cog numbers
    c := cog[i] := cognew(@entry, 0)
    if c=>0
      i++
  while c => 0
  count:=i 'return the count
  repeat 'unloads the cogs and updates the string
    i--
    if i=>0
      cstr[cog[i]]:=cog[i]+48
      cogstop(cog[i])
  while i=>0      

PUB freestring : fstr
''returns the string of the cogs free (numbered) and in use (dotted)
  freecount 'calls freecount to update the string
  fstr := @cstr

DAT

                        org
'
' Entry
'
entry                   jmp     entry                   'just loops

cstr                    byte    "••••••••",0