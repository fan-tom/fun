fls=\x.\y.y;
tru=\x.\y.x;
and=\l.\r.((l r) l);
or=\l.\r.((l l) r);
not=\l.((l fls) tru);
((or tru) (not tru));
((or ((and tru) (not fls))) ((and fls) (not tru)));