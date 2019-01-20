# conway-wechsler
Command line utility - Convert numbers into a pronounceable form.

# What does it solve?
In English, numbers are pronounced by splitting their digits into sets of three. These sets of three are then read as regular numerals with a suffix denoting the power of ten by which the set is multiplied.

For example, `12,150,000,023` is split into `012` and `153` and `000` and `023` respectively
12,150,000,023 = 
12  * 10^9
150 * 10^6
000 * 10^3
23
This split up text is then pronounced by replacing the powers of 10 by words
12  billion  (* 10^9)
150 million  (* 10^6)
000 thousand (* 10^3)
23

The table for converting these is as follows:
=== ONGOING ===

The issue is that there are no clear words for converting powers of 10^33 and beyond. With conventional words, we can only say numbers up to 10^30 (nonillion). However, Conway-Wechsler form allows us to go beyond that.

# What is Conway-Wechsler form?
The Conway-Wechsler system is a system set forth by John Horton Conway and Allan Wechsler [1] which proposes a clear way to create prefixes for any nth -illion.

[1] The Book of Numbers, Springer-Verlag, New York, 1996. ISBN 038797993X. 
