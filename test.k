dbg:{`0:x,`k@y}
nil:{{[v;k;e;as;cs](e;v,as;k,cs)}[x;y;;]}

cl0:{{[n;a;k;e;as;cs](e;(a@|n#as),n_as;k,cs)}[x;y;z;;]}
seq:{{{y[x]}/[(*|x)[y];|-1_x]}[x;y]}
cll:{seq[y,x]}

set:{{[n;k;e;as;cs]r:(@[e;n;:;as];1_as;k,cs);r}[x;y]}
get:{{[n;k;e;as;cs](e;(e[n]),as;k,cs)}[x;y]}

ifb:{[th;el;k;e;as;cs](e;1_as;$[*as;th[k];el[k]],cs)}
stc:{[t;th;k;e;as;cs]$[t@*as;(e;1_as;th[k],cs);(e;as;k,cs)]}
wh:{[pr;bd;k;e;as;cs]{[pr;bd;k;e;as;cs]r:seq(pr;ifb[seq(bd;o[pr;bd]);nil[99]]);r[k][e;as;cs]}[pr;bd;k;e;as;cs]}

and:{[a;b]seq(a;stc[(::);b])}
or:{[a;b]seq(a;stc[~:;b])}

blk:{nil[x]}
cal:{[k;e;as;cs]r:(e;1_as;(*as)[k],cs);r}

run:{{#*|x}{(e;as;cs):x;dbg["run:";as];(*cs)[e;as;1_cs]}/(x;();,y[z])}

k1:{(x;y;z)};env:`dummy!`dummy

f:cl0[2;+/];a:(nil[3];nil[2])
run[env;cll[f;a];k1]

f:set[`a];a:,nil[7]
run[env;cll[f;a];k1]

code:seq(cll[f;a];get[`a])
run[env;code;k1]

code:seq(nil[0];ifb[nil[2];nil[3]])
run[env;code;k1]

pr:get[`n]
wb:seq(cll[cl0[2;-/];(get[`n];nil[1])];set[`n])

code:seq(nil[10];set[`n];wh[pr;wb])
run[env;code;k1]

`0:"and 0 7"
code:and[nil[0];nil[17]]
run[env;code;k1]
`0:"or 0 17"
code:or[nil[0];nil[17]]
run[env;code;k1]
`0:"and 1 7"
code:and[nil[1];nil[7]]
run[env;code;k1]
`0:"or 1 17"
code:or[nil[1];nil[7]]
run[env;code;k1]

code:seq(blk[nil[1]];cal)
run[env;code;k1]
