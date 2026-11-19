import json,os,urllib.request as u
T=[(ord(c)-107)*5 for c in "kkkkkkkklmmggmmlljikkijlkkkookkkllmppmllmmoqqommuuuuuuuukkkkkkkkaceeeecacgkllkgcelmnnmleeknoonkeelnoonleekmnnmkecgkkkkgcaceeeecaoqmkkmqoookkkkooiggggggigeecceegeccaacceeccaacceeccaacceeccaacce"]
V=[0,100,320,330,500,900,0];P=[0,0,1,1,1,1,2];R={0:2,7:1,112:8,119:4}
D={2:(-33,-31,-18,-14,14,18,31,33),6:(-17,-16,-15,-1,1,15,16,17),3:(-17,-15,15,17),4:(-16,-1,1,16)}
B=[0]*128;S=1;C=0;W=4;X=116
A=Exception
def F(f):
 global B,S,C,W,X
 q=f.split();B=[0]*128
 for r,row in enumerate(q[0].split("/")):
  x=0
  for ch in row:
   if ch.isdigit():x+=int(ch)
   else:
    s=(7-r)*16+x;p="PNBRQK".index(ch.upper())+1
    B[s]=-p if ch.islower() else p
    if p==6:
     if ch.islower():X=s
     else:W=s
    x+=1
 S=1 if q[1]=="w" else -1;C=0
 for i in range(4):
  if "KQkq"[i] in q[2]:C|=1<<i

def at(q,c):
 for d in (-16*c-1,-16*c+1):
  if (q+d)&136==0 and B[q+d]==c:return 1
 for d in D[2]:
  t=q+d
  if t&136==0 and B[t]==2*c:return 1
 for d in D[6]:
  t=q+d
  if t&136==0 and B[t]==6*c:return 1
 for d in D[3]+D[4]:
  t=q+d
  while t&136==0:
   p=B[t]
   if p:
    if p==5*c or p==(3 if d in D[3] else 4)*c:return 1
    break
   t+=d
 return 0
def G():
 m=[]
 for s in range(128):
  p=B[s]
  if p*S<=0:continue
  t=p if p>0 else -p
  if t==1:
   d=16*S;L=7 if S>0 else 0;R=1 if S>0 else 6
   for o in (d-1,d,d+1):
    v=s+o
    if v&136 or (B[v] if o==d else B[v]*S>=0):continue
    m.append(s|(v<<7)|((5<<14) if v>>4==L else 0))
    if o==d and (s>>4)==R and B[s+2*d]==0:m.append(s|((s+2*d)<<7))
  else:
   for d in D[t if t<5 else 6]:
    v=s+d
    while v&136==0:
     q=B[v]
     if q==0:m.append(s|(v<<7))
     else:
      if q*S<0:m.append(s|(v<<7))
      break
     if t%4==2:break
     v+=d
 for k,b,r in ((4,6,1),(4,2,2),(116,118,4),(116,114,8)):
  a=(k+b)//2
  if C&r and B[a]==0 and B[b]==0 and (b>k or B[b-1]==0)and B[k+3 if b>k else k-4]==4*S and not at(k,-S)and not at(a,-S)and not at(b,-S):m.append(k|(b<<7))
 return m
def M(m):
 global S,C,W,X
 f=m&127;t=(m>>7)&127;pr=(m>>14)&7;p=B[f]
 st=(B[:],S,C,W,X)
 if p==6:C&=~3;W=t
 elif p==-6:C&=~12;X=t
 for q in (f,t):C&=~R.get(q,0)
 B[f]=0
 B[t]=pr*S if pr else p
 if p*p==36 and abs(t-f)==2:
  a=f+3 if t>f else f-4;B[a]=0;B[f+1 if t>f else f-1]=4 if p>0 else -4
 S=-S
 return st
def rs(st):
 global B,S,C,W,X
 B[:],S,C,W,X=st

def ev():
 v=0
 for s in range(128):
  p=B[s]
  if p:
   t=abs(p);r=(s>>4) if p>0 else 7-(s>>4)
   v+=(V[t]+T[P[t]*64+(s&7)+r*8])*(p//t)
 v+=20*(W in (2,6))-20*(W==4 and not C&3)-20*(X in (114,118))+20*(X==116 and not C&12)
 return v*S
ND=0
def od(mv):
 mv.sort(key=lambda m:V[abs(B[(m>>7)&127])] if B[(m>>7)&127]else 0,reverse=True);return mv
def se(alp,be,de,pl):
 global ND;ND+=1
 if ND>=15e4:raise A
 ch=at(W if S>0 else X,-S);kg=W if S>0 else X
 if de<=0 and not ch:
  v=ev()
  if v>=be:return be
  if v>alp:alp=v
 a=alp;lg=0
 for m in od(G()):
  f=m&127;t=(m>>7)&127;w=B[t];pr=(m>>14)&7
  if de<=0 and not ch:
   if not w and not pr:continue
  d=f-kg
  if ch or f==kg or d%15==0 or d%17==0 or d%16==0 or (f>>4)==(kg>>4):
   st=M(m)
   if at(X if S>0 else W,S):rs(st);continue
  else:st=M(m)
  lg+=1;s=-se(-be,-a,de-1,pl+1);rs(st)
  if s>a:
   a=s
   if a>=be:break
 if de<=0:return a
 if lg==0:return -100000+pl if ch else 0
 return a
def mu(m):
 a="abcdefgh";f=m&127;t=(m>>7)&127;s=a[f&7]+str((f>>4)+1)+a[t&7]+str((t>>4)+1)
 if (m>>14)&7:s+="q"
 return s
def rt(fn,lu):
 global ND;F(fn);by={}
 for m in G():
  st=M(m)
  if not at(X if S>0 else W,S):by[mu(m)]=m
  rs(st)
 cd=[by[v] for v in lu if v in by]
 if not cd:return lu[0]
 bs=cd[0];ND=0;de=1
 while 1:
  try:
   b=None;a=alp=-100001;be=100001;od(cd)
   for m in cd:
    st=M(m);s=-se(-be,-alp,de-1,1);rs(st)
    if s>a:a=s;b=m
    if s>alp:alp=s
   bs=b or bs
   de+=1
  except A:break
 return mu(bs)
E=os.environ["MAKEFUN_ENV_URL"]
def pt(pa,bd):
 with u.urlopen(E+pa,json.dumps(bd).encode()) as r:return json.load(r)
while 1:
 g=pt("/game",{})
 if g["status"]=="done":break
 while g["status"]=="your_move":
  g=pt("/move",{"game":g["game"],"move":rt(g["fen"],g["legal_moves"])})
