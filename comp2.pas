unit comp2;

interface

uses
  Windows,Regcomp, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Graph, Db, DBTables, Menus;

type
  Tcomp = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    XYGraph1: TXYGraph;
    RecSet: TTable;
    ScrollBar1: TScrollBar;
    Edit3: TEdit;
    MainMenu1: TMainMenu;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    procedure XYGraph1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure XYGraph1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }
  end;

 const
 MaxBaseCost = 150;
 MaxMarkup=30;
 MinMarkup=-5;

  type
       arr=array [0..2] of integer;
       vector=array[1..100] of single;
       finalData=array[MinMarkup..MaxMarkup] of arr;
       dataArray=array [0..1,1..100] of single;
       VectorRec = record
          vec:vector;
          items: integer;
       end ;

  var
   comp: Tcomp;
   ans:arr;
   dataSet:dataArray;

 function sign(d:single):boolean;
 function fct(d:integer;c:VectorRec):single;
 function CutPoint(a:integer;b:integer;c:VectorRec):arr;
 function IdentityMatrix (const k : byte): XMatrix;
 function DeterminantMatrix (const Matrix : XMatrix;
                                     const k      : byte): extended;
 function CopyMatrix (const Matrix : XMatrix;
                              const k      : byte): XMatrix;
 function InitMatrix (const k      : byte;
                              const aValue : extended):XMatrix;
 function VectorInit(x:VectorRec;a:integer):vectorRec;
 function GetXValue(x:dataArray;a:VectorRec):VectorRec;
 function GetYValue(x:dataArray;a:VectorRec):VectorRec;
 function VectorSum(a:VectorRec):single;
 function VectorSquare(a:VectorRec):single;
 function VectorCube(a:VectorRec):single;
 function VectorForth(a:VectorRec):single;
 function VectorXYSum(a:VectorRec;b:VectorRec):single;
 function VectorX2YSum(a:VectorRec;b:VectorRec):single;
 function MatrixReplace(a:XMatrix;b:VectorRec;col:integer):XMatrix;
 function CurveFind(a:XMatrix;b:VectorRec;c:VectorRec):VectorRec;
 function CompVector(a:VectorRec;b:VectorRec):VectorRec;
 function MultVector(a:VectorRec;b:single):VectorRec;
 procedure CompPaint(a:finaldata;b:TColor;c:string);
 procedure Start(cut:boolean);
 procedure EraseCurve;
 function FindMax(c:VectorRec):single;
 function FindMin(c:VectorRec):single;

 implementation

uses  Unit2,unit6;
{$R *.DFM}

// function section
procedure Start(cut:boolean);
var
    a,i,k:integer;
    s: string;
    XVector:VectorRec;
    YVector:VectorRec;
    CurveVector:VectorRec;
    BVector:VectorRec;
    Matrix:XMatrix;
    arrdata:finaldata;
    AColor:TColor;
begin
         XVector.items:=comp.REcSet.RecordCount;
         XVector:=Vectorinit(XVector,XVector.items);
         XVector:=GetXValue(dataSet,XVector);

         YVector.items:=comp.REcSet.RecordCount;
         YVector:=Vectorinit(YVector,YVector.items);
         YVector:=GetYValue(dataSet,YVector);

          if cut=true then
           XVector:=MultVector(XVector,(0.9));

       a:=0;
       for k:=0 to 3 Do         // -5,0,5,10 competitiveness
        begin
        for i:= MinMarkup to MaxMarkup Do      //mark-up range
         begin
         BVector:=MultVector(XVector,(1+i/100));
         BVector:=CompVector(BVector,YVector);
         CurveVector.items:=3;       // eqt variables a,b,c
         CurveVector:=Vectorinit(CurveVector,CurveVector.items);
         CurveVector:=CurveFind(Matrix,XVector,BVector);
         AColor:=clBlue;
         case k of
           0 : begin s:='0% comp';end;
           1 : begin s:='5% comp';AColor:=clRed;CurveVector.vec[3]:=CurveVector.vec[3]-5; end;//5% ccompetitiveness
           2 : begin s:='10% comp';AColor:=clYellow;CurveVector.vec[3]:=CurveVector.vec[3]-10; end;//10% ccompetitiveness
           3 : begin s:='15% comp';AColor:=clGreen;CurveVector.vec[3]:=CurveVector.vec[3]-15; end;//15% ccompetitiveness
         end; //end case
           arrdata[i]:=cutpoint(a, MaxBaseCost,CurveVector);
         end; //end i
        CompPaint(arrdata,AColor,s);
     end; //end j
end;

procedure Tcomp.FormCreate(Sender: TObject);      //get records
var
   i:integer;
   cut:boolean;
begin

   RecSet.open;
   RecSet.first;

   for i :=1 to REcSet.RecordCount do
    begin
         DataSet[0,i]:=RecSet.FieldByName('basecost').asFloat;
         DataSet[1,i]:=RecSet.FieldByName('lowbid').asFloat;
         RecSet.next;
    end;
       comp.XYGraph1.HintPanel.Strings.Add('0% cut curve');
        cut:=false;
        start(cut);
end;

procedure CompPaint(a:finaldata;b:TColor;c:string);
  var i,j,H:integer;
begin
  H:=comp.XYGraph1.MakeCurve(c,b,1,psSolid,True);
       for j:=MinMarkup to MaxMarkup do
       for i:=0 to  MaxBaseCost do
       if a[j][0]=i then comp.XYGraph1.AddPoint(H,i,j);
       comp.XYGraph1.AddText(H,-5,10,-10,c,clWhite);
       comp.XYGraph1.Paint;

  H:=comp.XYGraph1.MakeCurve(c,b,1,psSolid,True);
       for j:=MinMarkup to MaxMarkup do
       for i:=0 to  MaxBaseCost do
       if a[j][1]=i then comp.XYGraph1.AddPoint(H,i,j);
       comp.XYGraph1.AddText(H,-5,10,-10,c,clWhite);
       comp.XYGraph1.Paint;
end;

function CompVector(a:VectorRec;b:VectorRec):VectorRec;
  var i:integer;
      d:VectorRec;
begin
  for i:= 1 to a.items Do
    d.vec[i]:=((a.vec[i]-b.vec[i])*100)/b.vec[i];
    result:=d
  end;

function MultVector(a:VectorRec;b:single):VectorRec;
var   i:integer;
      d:VectorRec;
begin
    d.items:=comp.REcSet.RecordCount;
    d:=Vectorinit(d,d.items);
  for i:= 1 to a.items Do
    d.vec[i]:=a.vec[i]*b;
    result:=d
end;

function CurveFind(a:XMatrix;b:VectorRec;c:VectorRec):VectorRec;
var  ZVector,e:VectorRec;
begin
         ZVector.items:=3;
         ZVector:=Vectorinit(ZVector,ZVector.items);

 // 2 nd degree curvlinear regression
a[1,1]:=b.items;        a[2,1]:=VectorSum(b);   a[3,1]:=VectorSquare(b) ;
a[1,2]:=VectorSum(b);   a[2,2]:=VectorSquare(b);a[3,2]:=VectorCube(b);
a[1,3]:=VectorSquare(b);a[2,3]:=VectorCube(b) ; a[3,3]:=VectorForth(b) ;

ZVector.vec[1]:=VectorSum(c);
ZVector.vec[2]:=VectorXYSum(b,c);
ZVector.vec[3]:=VectorX2YSum(b,c);

  e.vec[3]:=DeterminantMatrix(MatrixReplace(a,ZVector,1),3)/DeterminantMatrix(a,3);
  e.vec[2]:=DeterminantMatrix(MatrixReplace(a,ZVector,2),3)/DeterminantMatrix(a,3);
  e.vec[1]:=DeterminantMatrix(MatrixReplace(a,ZVector,3),3)/DeterminantMatrix(a,3);

  result:=e;
end;

function CutPoint(a:integer;b:integer;c:VectorRec):arr;
var
    i,swap:integer;
    ans:arr;
    s,e,m:single;
    toggle:boolean;

begin
    ans[0]:=999;                //999 is marker for none
    ans[1]:=999;
    toggle:=false;

    //keep curves (left & right in continous
    if Sign(fct(a,c))=true then  //positive  curve : case 1
    begin
    m:=FindMax(c);
    e:=fct(MAXBASECOST,c);
    s:=fct(0,c);
    if (m>s) and (m>e) and (s>e) then toggle:=true;
    end;

    if Sign(fct(a,c))=false then  //negative curve : case 2
    begin
    m:=FindMin(c);
    e:=fct(MAXBASECOST,c);
    s:=fct(0,c);
    if (e>s) and (e>m) and (s>m) then toggle:=true;
    end;

    for i:=1 to b  do             //scan from left to right
     begin
      if sign(fct(i,c)) <> sign(fct(a,c)) then
       begin
        if ans[0]=999 then
               begin
                 ans[0]:=i; a:=i;        //find second
               end
         else
               begin
                  ans[1]:=i; break;     //get first and quit
               end;
         end;

      end;

    if toggle then //if store in wrong curve, change it
    begin
    swap:=ans[0];
    ans[1]:=swap;
    ans[0]:=999;
    end;

    result:=ans;
end;

function FindMax(c:VectorRec):single;
var i:integer;
    max:single;
begin
  max:=0;
  for i:=1 to MaxBaseCost Do
      if max<fct(i,c) then max:=fct(i,c);
   result:=max;
end;

function FindMin(c:VectorRec):single;
var i:integer;
    min:single;
begin
  min:=0;
  for i:=1 to MaxBaseCost Do
      if min>fct(i,c) then min:=fct(i,c);
   result:=min;
end;

function fct(d:integer;c:VectorRec):single;
begin
   result:= c.vec[1]*sqr(d)+c.vec[2]*d+c.vec[3];
end;

function sign(d:single):boolean;
begin
 if d<0 then result:= false
 else result:= true;
end;

function MatrixReplace(a:XMatrix;b:VectorRec;col:integer):XMatrix;
  var i:integer;
begin
  for i:= 1 to 3 Do
    a[col,i]:=b.vec[i];
    result:=a;
end;

function VectorSum(a:VectorRec):single;
  var i:integer;
      sum: single;
begin
  sum:=0;
  for i:= 1 to a.items Do
    sum:=sum+a.Vec[i];
    result:=sum;
end;

function VectorSquare(a:VectorRec):single;
  var i:integer;
      square: single;
begin
  square:=0;
  for i:= 1 to a.items Do
    square:=square+sqr(a.Vec[i]);
    result:=square;
end;

function VectorXYSum(a:VectorRec;b:VectorRec):single;
  var i:integer;
      XY: single;
begin
  XY:=0;
  for i:= 1 to a.items Do
    XY:=XY+a.Vec[i]*b.Vec[i];
    result:=XY;
end;

function VectorX2YSum(a:VectorRec;b:VectorRec):single;
  var i:integer;
      X2Y: single;
begin
X2Y:=0;
  for i:= 1 to a.items Do
    X2Y:=X2Y+sqr(a.Vec[i])*b.Vec[i];
    result:=X2Y;
end;

function VectorCube(a:VectorRec):single;
  var i:integer;
      cube: single;
begin
cube:=0;
  for i:= 1 to a.items Do
    cube:=cube+sqr(a.Vec[i])*a.Vec[i];
    result:=cube;
end;

function VectorForth(a:VectorRec):single;
  var i:integer;
      forth: single;
begin
forth:=0;
  for i:= 1 to a.items Do
    forth:=forth+sqr(a.Vec[i])*sqr(a.Vec[i]);
    result:=forth;
end;

function GetXValue(x:dataArray;a:VectorRec):VectorRec;
 var i:integer;
 begin
  for i:= 1 to a.items Do
    a.vec[i]:=x[0,i];
    result:=a;
end;

function GetYValue(x:dataArray;a:VectorRec):VectorRec;
 var i:integer;
 begin
  for i:= 1 to a.items Do
    a.vec[i]:=x[1,i];
    result:=a;
end;

function VectorInit(x:VectorRec;a:integer):VectorRec;
 var i:integer;
 begin
  for i:= 0 to a-1 Do
    x.vec[i]:=0;
    result:=x;
end;

 // matrix operation
function DeterminantMatrix (const Matrix : XMatrix;
                                     const k      : byte): extended;
var
  i: byte;
  j: byte;
  l: byte;
  dDiver: extended;
  dDet: extended;
  dRatio: extended;
  MatrixOut: XMatrix;
  MatrixIn: XMatrix;
begin

  MatrixOut   := IdentityMatrix(k);   // 'create' identity (k x k)
  MatrixIn    := CopyMatrix(Matrix,k);

  dDet := 1.0;                        // determinant

  for i := 1 to k do begin            // ok, do it
    dDiver := MatrixIn[i,i];
    dDet   := dDet * dDiver;

    for j := 1 to k do begin
      try
        MatrixIn[i,j]:= MatrixIn[i,j] / dDiver;
        MatrixOut[i,j]:= MatrixOut[i,j] / dDiver;
      except
        Raise Exception.Create('Error div by 0 inverting matrices.');
      end;
    end;

    for j := 1 to k do begin
      if ( i-j <> 0 ) then begin
        dRatio:= MatrixIn[j,i];
        for l := 1 to k do begin
          MatrixIn[j,l]:= MatrixIn[j,l] - ( dRatio * MatrixIn[i,l] );
          MatrixOut[j,l]:= MatrixOut[j,l] - ( dRatio * MatrixOut[i,l] );
        end;
      end;
    end;
  end;

  Result:= dDet;     // return Result

end;

function IdentityMatrix (const k : byte): XMatrix;
var
  i,j: byte;
begin

  Result:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
     for j:= 1 to k do
        Result[i,j]:= 0.0;

  for i:= 1 to k do
     Result[i,i]:= 1.0;

end;

function CopyMatrix (const Matrix : XMatrix;
                              const k      : byte): XMatrix;
var
  i,j: byte;
begin

  Result:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
     for j:= 1 to k do
        Result[i,j]:= Matrix[i,j];

end;

 function InitMatrix (const k      : byte;
                              const aValue : extended):XMatrix;

var
  i: byte;
  j: byte;
  MatrixOut: XMatrix;

begin

  for i:= 1 to MAXVARIABLES+1 do
    for j:= 1 to MAXVARIABLES+1 do
      MatrixOut[i,j]:= aValue;

  Result:= MatrixOut;

end;

procedure Tcomp.XYGraph1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var a,b,c,d:single;
begin
inherited MouseMove(Shift,X,Y);
a:=XYGraph1.XAxis.Min;
b:=XYGraph1.XAxis.Max;
c:=XYGraph1.YAxis.Min;
d:=XYGraph1.YAxis.Max;
edit1.text:=floatTostr(((X-60)/(390/(b-a)))+a);    //axis coordinateion transformation
edit2.text:=floatTostr((-Y+255)/(224/(d-c))+c);
end;

procedure Tcomp.XYGraph1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var a,b,c,d:single;
      cut:boolean;
      i:integer;
begin
inherited MouseDown(Button,Shift,X,Y);
 if Button = mbleft then
  begin
  a:=XYGraph1.XAxis.Min;
  b:=XYGraph1.XAxis.Max;
  c:=XYGraph1.YAxis.Min;
  d:=XYGraph1.YAxis.Max;

  XYGraph1.XAxis.SetCenter(((X-60)/(390/(b-a)))+a);
  XYGraph1.YAxis.SetCenter((-Y+255)/(224/(d-c))+c);
  XYGraph1.SetZoom(0.9);
   end;

  if Button = mbright then
  begin
      EraseCurve;
      //set predefined co-ordinates
      comp.XYGraph1.XAxis.Min:=0;
      comp.XYGraph1.XAxis.Max:=MaxBaseCost;
      comp.XYGraph1.YAxis.Min:=MinMarkup;
      comp.XYGraph1.YAxis.Max:=MaxMarkup;
      comp.XYGraph1.HintPanel.strings.Clear;
      comp.XYGraph1.HintPanel.Strings.Add('0% cut curve');
      comp.XYGraph1.repaint;
        cut:=false;
        start(cut);
  end;
end;

procedure Tcomp.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
  var H:integer;
begin
   edit3.text:=floatToStr(scrollbar1.position/100);  // prompt select base cost
   if scrollcode=scEndScroll then
   begin
   H:=comp.XYGraph1.MakeCurve('',clWhite,1,psSolid,True);
        comp.XYGraph1.AddPoint(H,scrollbar1.position/100,Xmarkup);
        comp.XYGraph1.AddMark(H,0,mtCross,clWhite);
        comp.XYGraph1.Paint;
    end;
end;

procedure EraseCurve;
var i,j:integer;
begin
   for i:= 0 to 3 Do          //?
       for j:=0 to 3 Do
     comp.XYGraph1.DeleteCurve(j);
end;

procedure Tcomp.Button1Click(Sender: TObject);
var
  cutcost:boolean;
begin
        comp.XYGraph1.HintPanel.strings.Clear;
        comp.XYGraph1.HintPanel.Strings.Add('cut 10% curve');
        cutcost:=true;
        eraseCurve;
        start(cutcost);
end;

procedure Tcomp.Exit1Click(Sender: TObject);
begin
Application.Terminate;
end;

procedure Tcomp.Help1Click(Sender: TObject);
begin
form6.visible:=true;
end;

end.
