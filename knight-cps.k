/----------- lexer -----------/
up:`c$"A"+!26
dg:`c$"0"+!10
fn:":`!~+-*/%^<>?&|;="
lt:"$,.@\\"
cl:@[&"a"+256;c;:;1+!#c:("'";"\"";"\n";"#";" \t\r{}[]()";"_";fn;up;_up;dg;lt)]
CL:`XX`st`st`sp`cm`sp`vr`FN`fn`vr`nm@cl
tr:(*'tb)?/:1_'tb:1_'"\n"\1_"
 uuuuuuuuuuuuu
 ssSssssssssss
 SusdwcwiJIinu
 dddDddddddddd
 DusdwcwiJIinu
 ccccCcccccccc
 CusdwcwiJIinu
 IusdwcwIJIinu
 JusdwcwiJIinu
 iusdwcwiJIiiu
 nusdwcwiJIinu
 wusdwcwiJIinu"

rm:(*'tb)?"SDcCw"
tok:{(*:';::)@'(&~=':r)_/:(r:2{tr[x;y]}\cl@x;x)}
dsp:{{x,$[|/rm=y;();`FN=t:CL@*z;`fn,/:z;`st=t;,`st,,1_z;`nm=t;,`nm,10/z-"0";,(t;z)]}/[()].(tok x)}

/----------- parser -----------/
stc:{(t;v):x;$[`st~t;x;`nl~t;(`st;"null");`bl~t;(`st;("false";"true")v);(`st;$[~v;"0";((0>*s)#"-"),`c$"0"+10\*|s:sgn@v])]}
blc:{(t;v):x;$[`st~t;(`bl;~~#v);`nl~t;(`bl;0);`bl~t;x;(`bl;~0=v)]}
s2n:{r*~^r:(1 -1"-"=*x)*`I$#/|(,t),+/&\~"0:"'t:(|/"+-"=*x)_x:(&\~^" \n\r\t"?)_x}
nmc:{(t;v):x;$[`st~t;(`nm;s2n@v);`nl~t;(`nm;0);`bl~t;(`nm;v);x]}
cnv:`st`bl`nm!(stc;blc;nmc)
sgn:{(s[1];*/s:1(1 -1@0>)\x)}
dbg:{`0:x,`k@y}
MXRND:32767
`prng@0
FNS:*''+(("TRUE";0;{y;(x;(`bl;1))})
 ("FALSE"; 0;{y;(x;(`bl;0))})
 ("NULL";  0;{y;(x;(`nl;()))})
 ("PROMPT";0;{y;p:_/|1((-+/&\0 1 ="\n\r"?)@|-2#)\1:1;(x;(`st;p))})
 ("RANDOM";0;{y;(x;(`nm;*1?MXRND))})
 (,":";    1;{(x;*y)})
 ("EVAL";  1;{})
 ("BLOCK"; 1;{})
 ("CALL";  1;{})
 (,"`";    1;{[e;a];(t;s):*a;(e;(`st;"\n"/."\\",s))})
 ("QUIT";  1;{})
 (,"!";    1;{[e;a];(t;b):blc(*a);(e;(`bl;~b))})
 ("LENGTH";1;{[e;a];(t;s):*a;(e;#s)})
 ("DUMP";  1;{[e;a];(t;v):r:*a;(`st;,/(((`nl`bl`st`nm!("Null";"Boolean";"String";"Number")))t;,"(";*|stc@r;,")"))})
 ("OUTPUT";1;{[e;a];(t;s):stc(*a);`1:$["\\"=*|s;-1_s;s,"\n"];(e;(`nl;()))})
 ("ASCII"; 1;{[e;a];(t;b):*a;(e;(`st`nm@t~`st;$[`st~t;0+*b;`nm~t;`c$b;`err@"ASCII"]))})
 (,"~";    1;{[e;a];(e;(`nm;-:*|nmc@*a))})
 (,"+";    2;{(a;b):y;v:*|+(a;(cnv@*a)b);(x;(*a;((+/;,/)`st~*a)@v))})
 (,"-";    2;{(a;b):y;v:*|+(a;(cnv`nm)b);(x;(`nm;-/v))})
 (,"*";    2;{(a;b):y;v:*|+(a;(cnv`nm)b);(x;(*a;@[(*/;{(y*#x)#x}.);`st~*a]v))})
 (,"/";    2;{(a;b):y;v:*|+(a;(cnv`nm)b);(s;r):sgn@%/v;(x;(`nm;s*_r))})
 (,"%";    2;{e:x;(a;b):y;v:*|+(a;(cnv`nm)b);(x;y):v;(s;x):sgn@x;(e;(`nm;s*(y!x)))})
 (,"^";    2;{(a;b):y;v:*|+(a;(cnv`nm)b);(x;(`nm;*/#/|:v))})
 (,"<";    2;{(a;b):y;v:*|+(a;(cnv@*a)b);(x;(`bl;@[(</;</;*>:);`nm`bl`st?*a]v))})
 (,">";    2;{(a;b):y;v:*|+(a;(cnv@*a)b);(x;(`bl;@[(>/;>/;*>:);`nm`bl`st?*a]v))})
 (,"?";    2;{[e;a];(e;(`bl;~/a))})
 (,"&";    2;{})
 (,"|";    2;{})
 (,";";    2;{})
 (,"=";    2;{})
 ("WHILE"; 2;{})
 ("IF";    3;{})
 ("GET";   3;{[e;a](e;(`st;@[*|stc@*a;(*sl)+!*|sl:*|+nmc'1_a]))})
 ("SUBSTITUTE";4;{[e;a](s;n):(*|+)'(stc';nmc')@'a(0 3;1 2);(e;(`st;?[s[0];+\n;s[1]]))}))

art:!/2#FNS
FN:!/FNS[0 2]

prs:{r:{$[~`fn~*y;(,y),x;(#x)<a:art@**|y;`err@"not enough args:",*|y;(,(y,,a#x)),a_x]}/[();|dsp@x];$[1<#r;`err@"spurious code";*r]}
nil:{{[v;k;e;as](e;as,,v;k)}[x;y;;]}
dup:{[k;e;as](e;as,,*as;k)}
chp:{[k;e;as](e;-1_as;k)}
nul:{[k;e;as](e;as;k)}

cl0:{{[n;a;k;e;as];(e;r):a[e;(-n)#as];;(e;((-n)_as),,r;k)}[x;y;z;;]}
seq:{{{y[x]}/[(*|x)[y];|-1_x]}[x;y]}
cll:{seq[y,x]}

set:{{[n;k;e;as](@[e;n;:;,*|as];as;k)}[x;y]}
get:{{[n;k;e;as](e;as,e[n];k)}[x;y]}

ifb:{[th;el;k;e;as](e;-1_as;$[(`bl;1)~blc(*|as);th;el][k])}
wh:{[pr;bd;k]r:seq(pr;ifb[seq(bd;chp;o[pr;bd]);nil[(`nl;())]]);r[k]}

and:{[a;b]seq(a;dup;ifb[seq(chp;b);nul])}
or:{[a;b]seq(a;dup;ifb[nul;seq(chp;b)])}

cal:{[k;e;as](e;-1_as;(*|as)[k])}
evl:{[k;e;as]c:stc(*|as);(e;-1_as;(cmp@prs[*|c])[k])}
qt:{[k;e;as](e;,(`qt;*|*|as);())}

CTRL:!/+((";";{(a;b):cmp'x;seq(a;chp;b)})
  ("I";{(p;t;e):cmp'x;seq(p;ifb[t;e])})
  ("W";{(p;b):cmp'x;wh[p;b]})
  ("=";{(n;v):x;seq(cmp[v];set[,*|n])})
  ("&";{and.(cmp'x)})
  ("|";{or.(cmp'x)})
  ("B";{nil@cmp[*x]})
  ("C";{seq(cmp[*x];cal)})
  ("E";{seq(cmp[*x];evl)})
  ("Q";{seq(cmp[*x];qt)}))

cmp:{$[|/`st`nm=t:*x;:nil[x];`vr~t;:get[,*|x];^(!FN)?*n:x[1];`err@n;~^(!CTRL)?*n;:CTRL[*n][x[2]];cll[cl0[#x[2];FN[*n]];o'x[2]]]}
run:{{#*|x}{(e;as;c):x;c[e;as]}/(x;();y[z])}

$[#x;**1_run[()!();cmp@prs@1:*x;{*y}];]
