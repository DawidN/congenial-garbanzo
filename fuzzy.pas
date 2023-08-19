unit fuzzy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, math,  Menus, DBTables, Db;

type
  TForm1 = class(TForm)
    Next: TButton;
    min: TComboBox;
    max: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox4: TComboBox;
    Button1: TButton;
    ComboBox3: TComboBox;
    Button2: TButton;
    MainMenu1: TMainMenu;
    INPUTdevice1: TMenuItem;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Label3: TLabel;
    Label4: TLabel;
    About2: TMenuItem;
    Edit1: TEdit;
    RadioButton1: TRadioButton;
    Exit1: TMenuItem;
    factable: TTable;
    objtable: TTable;
    Database1: TDatabase;
    Query1: TQuery;
    Query2: TQuery;
    Table1: TTable;
    Table2: TTable;
    CompetitivenessAnalysis1: TMenuItem;

    procedure NextClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure about1Click(Sender: TObject);
    procedure help1Click(Sender: TObject);
    procedure INPUTdevice1Click(Sender: TObject);
    procedure About2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure minDblClick(Sender: TObject);
    procedure ComboBox4DblClick(Sender: TObject);
    procedure ComboBox3DblClick(Sender: TObject);
    procedure CompetitivenessAnalysis1Click(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }
    end;

  Type
   ObjStr = record
   Obj : string;
   Strength: real;
  end ;

  Type
   FacStr = record
   Fac : string;
   Strength: real;
   end ;

  markup_range = array[1..6] of real;
  Type
   ObjFac = record
   Obj_Fac : String;
   SOF: real;
   M_range: markup_range;
  end ;

   type
   QOM =record
   QOM_id:string;
   Str:real;
   end;

  QOMRec =array [1..6,1..5] of QOM;
  FacRec = array[1..5] of FacStr;
  ObjRec = array[1..5] of ObjStr;
  ObjFacRec = array[1..5,1..5] of ObjFac;

  var
  Form1: TForm1;
  ObjToStr : ObjRec ;
  FacToStr : FacRec ;
  ObjFacRel : ObjFacRec ;
  QOMRel : QOMRec;
  FacCount: integer;
  ObjCount : integer;
  Mark_range : markup_range;

implementation

uses  Unit3, Unit4, Unit5, about, Unit6,comp2;

{$R *.DFM}

procedure TForm1.NextClick(Sender: TObject);
   var i,j,k: integer;
      m,n : real;
      s:string;
begin
 form3.stringGrid1.colcount:=FacCount;
 form3.stringGrid1.rowcount:=ObjCount;
    ObjCount:=ObjCount-1;
    FacCount:=FacCount-1;
for i:=1 to ObjCount do
    form3.stringGrid1.cells[0,i]:=ObjToStr[i].Obj;
for j:=1 to FacCount do
    form3.stringGrid1.cells[j,0]:=FacToStr[j].Fac;

  for i:=1 to ObjCount do
   begin
        for j:=1 to FacCount do
        begin
         ObjFacRel[j,i].Obj_Fac:=ObjToStr[i].Obj+ ' : '+ FacToStr[j].Fac;
         ObjFacRel[j,i].SOF:=ObjToStr[i].strength*FacToStr[j].strength;
        end;
  end;

if min.itemindex=-1 then m:=2 else
m:=StrTofloat(min.Items.Strings[min.itemindex]);
if max.itemindex=-1 then n:=12 else
n:=StrTofloat(max.Items.Strings[max.itemindex]);

{set mark up at form3}
 for k:=1 to 6 do
 begin
 mark_range[k]:=m+((k-1)*(n-m)/5);
   Str(mark_range[k]:0:2,s);
  form5.stringGrid1.cells[k,0]:=s;
end;
form1.visible:=false;
form3.visible:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
      var t,r,q:string;
begin
   if ObjCount >5 then ShowMessage('Objective cannot more than 5')
   else
   begin
   ObjToStr[ObjCount].Obj:= combobox4.text;
   ObjToStr[ObjCount].strength:=scrollbar1.position/10;
   ObjCount:=ObjCount+1;
   end;

//sql section
str(scrollbar1.position/10:3:1,t);
r:=combobox4.text;
q:='insert into obj (objective,str) values ('+''''+r+''''+','+t+');';
 query2.SQL.Append(q);
 query2.execsql;
 query2.SQL.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
 var s,q: string;
   i:integer;
begin
 min.selstart:=3;
 scrollbar1.position:=10;
 scrollbar2.position:=10;
 ObjCount:=1;
 FacCount:=1;

 q:='drop table newobj;';
 query2.SQL.Append(q);
 query2.execsql;
 query2.SQL.Clear;

 q:='drop table newfac;';
 query2.SQL.Append(q);
 query2.execsql;
 query2.SQL.Clear;

s:='SELECT TOP 3 obj.Objective, Sum(obj.str) AS SumOfstr, count(obj.objective) AS countOfstr INTO newobj FROM obj GROUP BY objective  ORDER BY sum(obj.str) DESC;';

query1.SQL.Append(s);
query1.execsql;
query1.SQL.Clear;

s:='SELECT TOP 5 fac.factor, Sum(fac.str) AS SumOfstr, count(fac.factor) AS countOfstr INTO newfac FROM fac GROUP BY factor  ORDER BY sum(fac.str) DESC;';

query1.SQL.Append(s);
query1.execsql;
query1.SQL.Clear;

objtable.open;
objtable.first;
for i :=1 to 3 do
begin
 combobox4.items.add(objtable.FieldByName('objective').asstring);
 objtable.next;
end;

factable.open;
objtable.first;
for i :=1 to 5 do
begin
combobox3.items.add(factable.FieldByName('factor').asstring);
 factable.next;
end;

end;

procedure TForm1.Button2Click(Sender: TObject);
var r,t,q:string;
begin
 if FacCount >5 then ShowMessage('Factors cannot more than 5')
   else
   begin
  FacToStr[FacCount].Fac:= combobox3.text;
  FacToStr[FacCount].strength:=scrollbar2.position/10;
  FacCount:=FacCount+1;
  end;


//sql section
str(scrollbar2.position/10:3:1,t);
r:=combobox3.text;
q:='insert into fac (factor,str) values ('+''''+r+''''+','+t+');';
 query2.SQL.Append(q);
 query2.execsql;
 query2.SQL.Clear;
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
label3.caption:=floatToStr(scrollbar1.position/10);
end;

procedure TForm1.ScrollBar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  label4.caption:=floatToStr(scrollbar2.position/10);
end;

procedure TForm1.about1Click(Sender: TObject);
begin
  aboutbox.visible:=true;
end;

procedure TForm1.help1Click(Sender: TObject);
begin
  Form6.visible:=true;
end;

procedure TForm1.INPUTdevice1Click(Sender: TObject);
begin
  form6.visible:=true;
end;

procedure TForm1.About2Click(Sender: TObject);
begin
  aboutbox.visible:=true;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
if Radiobutton1.checked=true then combobox3.Items.Add(edit1.text)
   else
   combobox4.Items.Add(edit1.text);
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
if Radiobutton1.checked=true then edit1.text:='Additional factors';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.minDblClick(Sender: TObject);
var i: integer;
     j,k :real;
begin
  objtable.open;
  objtable.first;
for i :=1 to 5 do
begin
 if objtable.FieldByName('objective').asstring=combobox4.text then
  begin
     j:=objtable.FieldByName('SumOfstr').asFloat;
     k:=objtable.FieldByName('countOfstr').asFloat;
     scrollbar1.position:=Ceil((j/k)*10);

  end;
  objtable.next;
end;
end;

procedure TForm1.ComboBox4DblClick(Sender: TObject);

var i: integer;
     j,k :real;
begin
  objtable.open;
  objtable.first;
for i :=1 to 3 do
begin
 if objtable.FieldByName('objective').asstring=combobox4.text then
  begin
     j:=objtable.FieldByName('SumOfstr').asFloat;
     k:=objtable.FieldByName('countOfstr').asFloat;
     scrollbar1.position:=Ceil((j/k)*10);
   end;
  objtable.next;
end;


end;



procedure TForm1.ComboBox3DblClick(Sender: TObject);

 var i: integer;
      j,k :real;
begin
factable.open;
factable.first;
for i :=1 to 5 do
begin
 if factable.FieldByName('factor').asstring=combobox3.text then
  begin
     j:=factable.FieldByName('SumOfstr').asFloat;
     k:=factable.FieldByName('countOfstr').asFloat;
     scrollbar2.position:=Ceil((j/k)*10);
   end;
  factable.next;
end;

end;

procedure TForm1.CompetitivenessAnalysis1Click(Sender: TObject);
begin
Comp.visible:=true;
end;

end.
