dbg:{`0:x,`k@y}
nil:{{[v;k;e;as;cs](e;v,as;k,cs)}[x;y;;]}

cl0:{{[n;a;k;e;as;cs](e;(a@n#as),n_as;k,cs)}[x;y;z;;]}
cll:{r:{{y[x]}/[(*x)[y];1_x]}[x;y];r}

set:{{[n;k;e;as;cs]r:(@[e;n;:;as];1_as;k,cs);r}[x;y]}
get:{{[n;k;e;as;cs]dbg["get:";(e;e[n])];(e;(e[n]),as;k,cs)}[x;y]}

f:cl0[2;+/];a:(nil[3];nil[2])

env:`dummy!2

run:{{#*|x}{(e;as;cs):x;(*cs)[e;as;1_cs]}/(x;();,y[z])}
run[env;cll[f,a];{(x;y;z)}]

f:set[`a];a:,nil[7]
run[env;cll[f,a];{(x;y;z)}]

seq:{cll[|x;y]}

k1:{(x;y;z)};sq1:(cll[f,a];get[`a]);sq1:(cll[f,a];get[`a])
run[env;seq[sq1];k1]

ifb:{[th;el;k;e;as;cs](e;1_as;$[*as;th[k];el[k]],cs)}

ifs:seq[(nil[0];ifb[nil[2];nil[3]])]
run[env;ifs;k1]

pr:get[`n]
wb:seq[(cll[(cl0[2;-/];get[`n];nil[1])];set[`n])]

wh:{[pr;bd;k;e;as;cs]{[pr;bd;k;e;as;cs]r:seq[(pr;ifb[seq[(bd;o[pr;bd])];nil[99]])];r[k][e;as;cs]}[pr;bd;k;e;as;cs]}

code:seq[(nil[10];set[`n];wh[pr;wb])]
run[env;code;k1]
