# conway-wechsler
Command line utility - Convert numbers into a pronounceable form.

[Usage](#usage)  
[Installation](#installation)  
[What does it do?](#what-does-it-do)  
[What is Conway-Wechsler form?](#what-is-conway-wechsler-form)

## Usage
```
Usage: conway-wechsler [flags]
  INPUT
  <n>: a number composed of digits
    -: read number composed of digits from stdin,
       If number is unspecified, this is default
  OUTPUT
   --newline,
   -n: newline between each -illion
   --keep,
   -k: express numbers < 1000 as numerals, not words
  MISCELLANEOUS
   --help,
   -h: show usage page
```

## Installation
1. Compile to an executable using GHC.   
   [(Getting started with Haskell)](https://haskell-lang.org/get-started)
2. Rename the executable to whatever you'd like to be called.
3. Put it in your PATH.  
   [(What is my PATH?)](http://www.linfo.org/path_env_var.html)
   
Please feel free to open an issue for help.

## What does it do?
In English, numbers are pronounced by splitting their digits into sets of three. These sets of three are then read as regular numerals with a suffix denoting the power of ten by which the set is multiplied.

For example, `12,150,000,023` is split into `012` and `153` and `000` and `023` respectively
```
12,150,000,023 = 
12  * 10^9
150 * 10^6
000 * 10^3
23
```
This split up text is then pronounced by replacing the powers of 10 by words
```
12  billion  (* 10^9)
150 million  (* 10^6)
000 thousand (* 10^3)
23
```

The table for converting these is as follows:
```
| power | word        |
|-------|-------------|
| 10^0  | (blank)     |
| 10^3  | thousand    |
| 10^6  | million     |
| 10^9  | billion     |
| 10^12 | trillion    |
| 10^15 | quadrillion |
| 10^18 | quintillion |
| 10^21 | sextillion  |
| 10^24 | septillion  |
| 10^27 | octillion   |
| 10^30 | nonillion   |
```

The issue is that there are no clear words for converting powers of 10^33 and beyond. With conventional words, we can only say numbers up to 10^30 (nonillion). However, Conway-Wechsler form allows us to go beyond that.

## What is Conway-Wechsler form?
The Conway-Wechsler system is a system set forth by John Horton Conway and Allan Wechsler [1] which proposes a clear way to create prefixes for any power of 10.

#### How to Use It

There is an excellent explanation by Robert Munafo [2] (CC BY-NC 4.0). I have reproduced it here in Markdown for your convenience.

```
| n |    ones    |       tens        |     hundreds      |
|---| ---------- | ----------------- | ----------------- |
| 0 |            |                   |                   |
| 1 | un         | (n) deci          | (nx) centi        |
| 2 | duo        | (ms) viginti      | (n) ducenti       |
| 3 | tre (*)    | (ns) triginta     | (ns) trecenti     |
| 4 | quattuor   | (ns) quadraginta  | (ns) quadringenti |
| 5 | quin       | (ns) quinquaginta | (ns) quingenti    |
| 6 | se (sx)    | (n) sexaginta     | (n) sescenti      |
| 7 | septe (mn) | (n) septuaginta   | (n) septingenti   |
| 8 | octo       | (mx) octoginta    | (mx) octingenti   |
| 9 | nove (mn)  | nonaginta         | nongenti          |
```

The rules for using the Conway-Wechsler system is as follows:
1. Take the power of 10 you're naming and subtract 3.
2. Divide by 3. If the remainder is 0, 1 or 2, put one, ten or one hundred at the beginning of your name (respectively).
3. For a quotient less than 10, use the standard names thousand, million, billion and so on through nonillion.

   Otherwise:
4. Break the quotient up into 1's, 10's and 100's. Find the appropriate name segments for each piece in the table.
5. String the segments together, inserting an extra letter if the letter shown in parentheses at the end of one segment match a letter in parentheses at the beginning of the next. 
   
   For example: septe(mn) + (ms)viginti = septemviginti because the (m)'s match; Another example: se(sx) + (mx)octoginta = sexoctoginta.
6. For the special case of tre, the letter s should be inserted if the following part is marked with either an s or an x.
7. Remove a final vowel, if any.
8. Add illion at the end. You're done. 

#### Extending to Arbitrary Values

The Conway-Wechsler system extends to arbitrarily high values. After setting out the rules above, the authors continue:
> With Allan Wechsler we propose to extend this system indefinitely by combining these according to the convention that "XilliYilliZillion" (say) denotes the (1000000X + 1000Y + Z)th zillion, using "nillion" for the zeroth "zillion" when this is needed as a placeholder. So for example the million-and-third zillion is a "millinillitrillion." 

## Citations

[1] The Book of Numbers, Springer-Verlag, New York, 1996. ISBN 038797993X.

[2] https://www.mrob.com/pub/math/largenum.html#conway-wechsler
