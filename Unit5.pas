unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Grids, StdCtrls;

type
  TForm5 = class(TForm)
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    Next: TButton;
    MainMenu1: TMainMenu;
    Help1: TMenuItem;
    procedure StringGrid1Click(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure Help1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

arr =array [1..5] of real;
function getmax(ex:arr):real;
function getmin(a:real;b:real):real;

var
  Form5: TForm5;
  minarray : arr;

implementation

uses fuzzy, Unit2, Unit6;

{$R *.DFM}

procedure TForm5.StringGrid1Click(Sender: TObject);
var i,j,k : integer;

begin
{right minus}
j:=stringGrid1.col;
k:=stringGrid1.row;
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='1.00';
ObjFacRel[stringGrid1.col,stringGrid1.row].M_range[j]:=1.00;

 //j:=stringGrid1.col;
 //k:=stringGrid1.row;

 {right minus}
 for i:=j+1 to 7 do
 begin
 stringGrid1.cells[i,k]:=floatToStr(StrTofloat(stringGrid1.Cells[i-1,k])-0.2);
 end;

  {left minus}
 while j>1  do
  begin
  stringGrid1.cells[j-1,k]:=floatToStr(StrTofloat(stringGrid1.Cells[j,k])-0.2);
  j:=j-1;
  end;

  end;

procedure TForm5.NextClick(Sender: TObject);
 var i,j,k,r: integer;
   s:String;
begin
r:=0;

try

for i:=1 to ObjCount Do
begin
  for j:=1 to FacCount Do
  begin
    for k:=1 to 6 Do
    begin
      ObjFacRel[j,i].M_range[k]:=StrToFloat(stringGrid1.Cells[k,r+j]);
    end;
  end;
    r:=r+FacCount;
end;

except
on E: Exception do raise EConvertError.CreateFmt('Please input value by mouse click.',[0,0,0]);
end ;

    {QOM built}
for j:=1 to ObjCount Do
begin
  for k:=1 to 6 Do
    begin
       for i:=1 to FacCount Do
     begin
       minarray[i]:=getmin(ObjFacRel[i,j].SOF,ObjFacRel[i,j].M_range[k]);
     end;

       QOMRel[k,j].STr:=getmax(minarray);
    end;
end;

// init stringGrid
form2.stringGrid1.Colcount:=7;
form2.stringGrid1.rowcount:=Objcount+2;
form2.StringGrid1.Cells[0,ObjCount+1]:='sum/sum(wt)';

for i:=1 to ObjCount do
  form2.StringGrid1.Cells[0,i]:=ObjToStr[i].Obj;

for i:=1 to 6 do
begin
  Str(mark_range[i]:3:1,s);
  form2.stringGrid1.cells[i,0]:=s;
end;

  for i:=1 to ObjCount do
    for j:=1 to 6 do
    begin
      Str(QOMRel[j,i].STr:3:2,s);
      form2.stringGrid1.cells[j,i]:=s;
    end;

form2.visible:=true;
form5.visible:=true;
end;

function getmin(a:real;b:real):real;
begin
 if a>=b then getmin:=b;
 if b>a then getmin:=a;
end;

function getmax(ex:arr):real;
var i:integer;
    m:real;
begin
m:=-999;
for i:=1 to 5 dO
  if ex[i]>=m then m:=ex[i];
  getmax:=m;
end;

procedure TForm5.Help1Click(Sender: TObject);
begin
form6.visible:=true;
end;

end.
