/----------- lexer -----------/
up:`c$"A"+!26
dg:`c$"0"+!10
fn:":`!~+-*/%^<>?&|;="
lt:"$,.@\\"
cl:@[&"a"+256;c;:;1+!#c:("'";"\"";"\n";"#";" \t\r{}[]()";"_";fn;up;_up;dg;lt)]
CL:`XX`st`st`sp`cm`sp`vr`fn`FN`vr`nm@cl
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
dsp:{{x,$[|/rm=y;();`fn=t:CL@*z;`fn,/:z;`st=t;,`st,,1_z;`nm=t;,`nm,10/z-"0";,(t;z)]}/[()].(tok x)}

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
 (,":";    1;{,/eva[x;y]})
 ("EVAL";  1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;s):*a;evl[e;prs s]})
 ("BLOCK"; 1;{(x;(`bk;*y))})
 ("CALL";  1;{r:eva[x;y];(e;a):$[`qt~*r;:r;r];$[`bk~*a:*a;evl[e;a[1]];:`err@"BLOCK"]})
 (,"`";    1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;s):*a;(e;(`st;"\n"/."\\",s))})
 ("QUIT";  1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;b):*a;(`qt;b)})
 (,"!";    1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;b):blc(*a);(e;(`bl;~b))})
 ("LENGTH";1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;s):*a;(e;#s)})
 ("DUMP";  1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;v):r:*a;(`st;,/(((`nl`bl`st`nm!("Null";"Boolean";"String";"Number")))t;,"(";*|stc@r;,")"))})
 ("OUTPUT";1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;s):stc(*a);`1:$["\\"=*|s;-1_s;s,"\n"];(e;(`nl;()))})
 ("ASCII"; 1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(t;b):*a;(e;(`st`nm@t~`st;$[`st~t;0+*b;`nm~t;`c$b;`err@"ASCII"]))})
 (,"~";    1;{(e;a):$[`qt~*r:eva[x;y];:r;r];(e;(`nm;-:*|nmc@*a))})
 (,"+";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv@*a)b);(e;(*a;@[(+/;,/);`st~*a]v))})
 (,"-";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv`nm)b);(e;(`nm;-/v))})
 (,"*";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv`nm)b);(e;(*a;@[(*/;{(y*#x)#x}.);`st~*a]v))})
 (,"/";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv`nm)b);(s;r):sgn@%/v;(e;(`nm;s*_r))})
 (,"%";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv`nm)b);(x;y):v;(s;x):sgn@x;(e;(`nm;s*(y!x)))})
 (,"^";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv`nm)b);(e;(`nm;*/#/|:v))})
 (,"<";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv@*a)b);(e;(`bl;@[(</;</;*>:);`nm`bl`st?*a]v))})
 (,">";    2;{(e;(a;b)):$[`qt~*r:eva[x;y];:r;r];v:*|+(a;(cnv@*a)b);(e;(`bl;@[(>/;>/;*>:);`nm`bl`st?*a]v))})
 (,"?";    2;{(e;a):$[`qt~*r:eva[x;y];:r;r];(e;(`bl;~/a))})
 (,"&";    2;{(a;b):y;(e;a):r:evl[x;a];$[(`qt~*r)|(`bl;0)~blc(a);r;evl[e;b]]})
 (,"|";    2;{(a;b):y;(e;a):r:evl[x;a];$[(`qt~*r)|(`bl;1)~blc(a);r;evl[e;b]]})
 (,";";    2;{r:eva[x;y];(e;(a;b)):$[`qt~*r;:r;r];(e;b)})
 (,"=";    2;{(a;b):y;(e;b):evl[x;b];$[`qt~e;:(e;b);];(@[e;a[1];:;b];b)})
 ("WHILE"; 2;{r:1_{1~*x}{$[`qt~*y[1];:0,1_y;];(p;b):x;e:*r:evl[y[1];p];$[`qt~e;:0,r;(`bl;0)~blc(r[1]);(0;e;y[2]);1,evl[e;b]]}[y]/(1;x;());$[`qt~*r;:r;(*r;(`nl;()))]})
 ("IF";    3;{(p;tn;el):y;e:*r:evl[x;p];$[`qt~e;:r;(`bl;1)~blc(r[1]);evl[e;tn];evl[e;el]]})
 ("GET";   3;{(e;a):$[`qt~*r:eva[x;y];:r;r];(e;(`st;@[*|stc@*a;(*sl)+!*|sl:*|+nmc'1_a]))})
 ("SUBSTITUTE";4;{(e;a):$[`qt~*r:eva[x;y];:r;r];(s;n):(*|+)'(stc';nmc')@'a(0 3;1 2);(e;(`st;?[s[0];+\n;s[1]]))}))

art:!/2#FNS
FN:!/FNS[0 2]
prs:{(tk;tr):{tk:*x;r:1_x;$[`fn=_*e:*tk;[rr:(0^art@*e:*|*tk)o/,1_tk;(,*rr),r,,(`fn;e;1_rr)];(,1_tk),r,,e]}@,dsp@x;
     $[#tk;`err@"incomplete parse:",`k@tk;];tr}
evl:{$[`qt~x;:(x;y);|/`st`nm`bl`nl`bk=t:*y;:(x;y);`vr~t;:$[^(!x)?y@1;`err@`k@^y@1;(x;x[y@1])]];evf[x;y]}
evf:{(t;n;a):y;$[^(!FN)?*n;:`err@"no func:",n;];FN[*n][x;a]}
eva:{r:{{$[`qt~*x;:x;`qt~**(e;v):evl[*x;y];:(e;v);e,1_x,,v]}/[,x;y]}[x;y];(*r;1_r:$[`qt~*r;:r;r])}

$[#x;2(*|:)/evl[()!()] prs@1:*x;]
