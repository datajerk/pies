: piatan ;

79 4 / constant maxl
1000 constant n
( NEW: m = n / log10 2^16 + 1 )
208 constant m
( s = size of int )
4 constant s
m s * constant e
create a e allot
create b e allot
create xx e allot
create yy e allot
variable d
variable f
variable ff
variable g
variable h

: 1d. <# # #> type ;
: 04d. <# # # # # #> type ;
: mpset over ! dup e + swap s + do 0 i ! s +loop ;
: mpcopy 2dup - swap dup e + swap do i @ over i + !  s +loop 2drop ;
: ud+ rot + rot rot + ;

: mpdiv
  d ! dup dup
  m 0 do dup @ 0= if s + else leave then loop
  dup e - rot = if drop 0 else
  0 swap rot e + swap do i @ swap d @ um/mod i !  s +loop
  drop 1
  then
;

: mpmult
  d ! dup dup e + s - m 0 do dup @ 0= if s - else leave then loop > if drop else
  0 swap dup e + s - do i @ d @ um* rot 0 d+ swap i !  s negate +loop drop then
;

: mpsub
  2dup - d !
  0 swap dup e + s - do i d @ + dup h ! @ 0 i @ 0 d- rot 0 ud+ h @ !
  s negate +loop
  2drop
;

: mpadd
  2dup - d !
  0 swap dup e + s - do i d @ + dup h ! @ 0 i @ 0 d+ rot 0 d+ swap h @ !
  s negate +loop
  2drop
;

: mpatan
  f ! g ! 1 ff ! xx 1 mpset xx f @ mpdiv drop g @ xx mpcopy
  f @ 1024 < if f @ dup * f ! 0 ff ! then
  1 begin 
    2 + xx f @ mpdiv drop
    ff @ 1 = if xx f @ mpdiv drop then
    yy xx mpcopy dup yy swap mpdiv 1 =
  while
    dup 2 and 0 > if g @ yy mpsub else g @ yy mpadd then
  repeat
  2drop
;

: mpprint
  xx swap mpcopy xx @ 0 1d. ." ." cr
  n 0 do
    0 xx ! xx 10000 mpmult xx @ 0 04d.
    i 4 / 1 + maxl mod 0= if cr then
  4 +loop
; 

: machin
  a 5 mpatan a 16 mpmult
  b 239 mpatan b 4 mpmult
  a b mpsub
;

: stormer
  a 57 mpatan a 176 mpmult
  b 239 mpatan b 28 mpmult a b mpadd
  b 682 mpatan b 48 mpmult a b mpsub
  b 12943 mpatan b 96 mpmult a b mpadd
;

machin a mpprint cr

