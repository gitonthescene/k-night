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
 IusdwcwIJIiiu
 JusdwcwiJIiiu
 iusdwcwiJIiiu
 nusdwcwiJIinu
 wusdwcwiJIinu"

rm:(*'tb)?"SDcCw"
tok:{(*:';::)@'(&~=':r)_/:(r:2{tr[x;y]}\cl@x;x)}
dsp:{{x,$[|/rm=y;();`fn=t:CL@*z;`fn,/:z;`st=t;,`st,,1_z;`nm=t;,`nm,10/z-"0";,(t;z)]}/[()].(tok x)}

/----------- parser -----------/
stc:{(t;v):x;$[`st~t;x;`nl~t;(`st;"null");`bl~t;(`st;("false";"true")v);(`st;$[~v;"0";(("";"-")0>*s),`c$"0"+10\*|s:sgn@v])]}
blc:{(t;v):x;$[`st~t;(`bl;~~#v);`nl~t;(`bl;0);`bl~t;x;(`bl;~0=v)]}
s2n:{r*~^r:(-1+2*~"-"=*x)*`I$#/|(,t),+/&\~"0:"'t:(|/"+-"=*x)_x:(&\~^" \n\r\t"?)_x}
nmc:{(t;v):x;$[`st~t;(`nm;s2n@v);`nl~t;(`nm;0);`bl~t;(`nm;v);x]}
cnv:`st`bl`nm!(stc;blc;nmc)
stub:{(x;y)}
math:{e:*r:eva[y;z];$[`qt~e;:r;];(t;v):+1_r;(e;(`nm;x[v]))}
sgn:{(s[1];*/s:1(-1+2*~0>)\x)}
MXRND:32767
`prng@0
FNS:*''+(("TRUE";0;{y;(x;(`bl;1))})
 ("FALSE";0;{y;(x;(`bl;0))})
 ("NULL";0;{y;(x;(`nl;()))})
 ("PROMPT";0;{y;(x;(`st;1:1))})
 ("RANDOM";0;{y;(x;(`nm;*1?MXRND))})
 (,":";1;{eva[x;y]})
 ("EVAL";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;s):*1_r;evl[e;prs s]})
 ("BLOCK";1;{(x;(`bk;*y))})
 ("CALL";1;{e:*r:eva[x;y];$[`qt~e;:r;~`bk~*r:*1_r;:`err@"BLOCK"];evl[e;r[1]]})
 (,"`";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;s):*1_r;evl[e;(`st;"\n"/."\\",s)]})
 ("QUIT";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;b):*1_r;(`qt;b)})
 (,"!";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;b):*1_r;(e;(`bl;~b))})
 ("LENGTH";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;s):*1_r;(e;#s)})
 ("DUMP";1;stub)
 ("OUTPUT";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;s):stc(*1_r);`1:$["\\"=*|s;-1_s;s,"\n"];(e;(`nl;()))})
 ("ASCII";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;b):*1_r;(e;$[`st~t;0+*b;`nm~t;`c$b;`err@"ASCII"])})
 (,"~";1;{e:*r:eva[x;y];$[`qt~e;:r;];(t;v):*1_r;(e;(`nm;-:*:v))})
 (,"+";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv@*a)b);(e;(*a;@[(+/;,/);`st~*a]v))})
 (,"-";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv`nm)b);(e;(`nm;-/v))})
 (,"*";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv`nm)b);(*a;@[(*/;{(y*#x)#x}.);`st~*a]v)})
 (,"/";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv`nm)b);(s;r):sgn@%/v;(e;(`nm;s*_r))})
 (,"%";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv`nm)b);(x;y):v;(s;x):sgn@x;(e;(`nm;s*(y!x)))})
 (,"^";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv`nm)b);(e;(`nm;*/#/|:v))})
 (,"<";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv@*a)b);(`bl;@[(</;</;*<:);`nm`bl`st?*a]v)})
 (,">";2;{e:*r:eva[x;y];$[`qt~e;:r;];(a;b):1_r;v:*|+(a;(cnv@*a)b);(`bl;@[(>/;>/;*>:);`nm`bl`st?*a]v)})
 (,"?";2;{e:*r:eva[x;y];$[`qt~e;:r;];(e;(`bl;~/1_r))})
 (,"&";2;{(a;b):y;(e;a):r:evl[x;a]$[(`qt~e)|(`bl;0)~blc(a);r;evl[e;b]]})
 (,"|";2;{(a;b):y;(e;a):r:evl[x;a]$[(`qt~e)|(`bl;1)~blc(a);r;evl[e;b]]})
 (,";";2;{e:*r:eva[x;y];$[`qt~e;:r;];(e;r[2])})
 (,"=";2;{(a;b):y;(e;b):evl[x;b];$[`qt~e;:(e;b);];(@[e;a[1];:;b];b)})
 ("WHILE";2;{r:1_{1~*x}{$[`qt~*y[1];:0,y[1];];(p;b):x;e:*r:evl[y[1];p];$[`qt~e;:0,r;(`bl;0)~blc(r[1]);(0;e;y[2]);1,evl[e;b]]}[y]/(1;x;());$[`qt~r;r;(*r;(`nl,()))]})
 ("IF";3;{(p;tn;el):y;e:*r:evl[x;p];$[`qt~e;:r;(`bl;1)~blc(r[1]);evl[e;tn];evl[e;el]]})
 ("GET";3;stub)
 ("SUBSTITUTE";4;stub))

art:!/2#FNS
FN:!/FNS[0 2]
prs:{(tk;tr):{tk:*x;r:1_x;$[~`fn=_*e:*tk;(,1_tk),r,,e;[rr:(0^art@*e:*|*tk)o/,1_tk;(,*rr),r,,(`fn;e;1_rr)]]}@,dsp@x;
     $[#tk;`err@"incomplete parse:",`k@tk;];tr}
evl:{$[`qt~t:*y;:y;|/`st`nm`bl`nl`bk=t;:(x;y);`vr~t;:$[^(!x)?y@1;`err@`k@^y@1;(x;x[y@1])]];evf[x;y]}
evf:{(t;n;a):y;$[^(!FN)?*n;:`err@"no func:",n;];FN[*n][x;a]}
eva:{{(e;v):evl[*x;y];$[`qt~e;:(e;v);];e,1_x,,v}/[,x;y]}

2(*|:)/$[#x;evl[(,,"x")!,(`nm;33)] prs@1:*x;]
