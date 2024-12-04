( This is currently just scratchpad for learning forth)
( Large letter F )
: STAR 42 EMIT ;
: STARS   0 DO  STAR LOOP ;
: MARGIN  CR 30 SPACES ;
: BLIP MARGIN STAR ;
: BAR  MARGIN 5 STARS ;
: F    BAR BLIP BAR BLIP BLIP CR ;

: 2c4 dup 1 + swap / . ;
: 2c5 dup 7 * 5 + * . ;
: 2c6 over 9 * swap - * . ;

( 1 yard = 36 inches )
( 1 foot = 12 inches )
: yards>in 36 * ;
: ft>in 12 * ;
: yards 36 * ;
: feet 12 * ;
: inches ;
           
