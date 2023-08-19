unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Grids, StdCtrls;

type
  Tform3 = class(TForm)
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    Strongly1: TMenuItem;
    Agree1: TMenuItem;
    netural1: TMenuItem;
    notlikely1: TMenuItem;
    notagree1: TMenuItem;
    Button2: TButton;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    NumericInputPad1: TMenuItem;
    Help1: TMenuItem;
    procedure Strongly1Click(Sender: TObject);
    procedure Agree1Click(Sender: TObject);
    procedure netural1Click(Sender: TObject);
    procedure notlikely1Click(Sender: TObject);
    procedure notagree1Click(Sender: TObject);
     procedure Button2Click(Sender: TObject);
    procedure NumericInputPad1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  form3: Tform3;

implementation

uses fuzzy, Unit5, Unit4, Unit6;

{$R *.DFM}

procedure Tform3.Strongly1Click(Sender: TObject);
begin
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='strongly';
end;

procedure Tform3.Agree1Click(Sender: TObject);
begin
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='Agree';
ObjFacRel[stringGrid1.col,stringGrid1.row].SOF:=ObjFacRel[stringGrid1.col,stringGrid1.row].SOF*0.75;
end;

procedure Tform3.netural1Click(Sender: TObject);
begin
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='Netural';
ObjFacRel[stringGrid1.col,stringGrid1.row].SOF:=ObjFacRel[stringGrid1.col,stringGrid1.row].SOF*0.5;
end;

procedure Tform3.notlikely1Click(Sender: TObject);
begin
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='Not Likely';
ObjFacRel[stringGrid1.col,stringGrid1.row].SOF:=ObjFacRel[stringGrid1.col,stringGrid1.row].SOF*0.25;
end;

procedure Tform3.notagree1Click(Sender: TObject);
begin
stringGrid1.Cells[stringGrid1.col,stringGrid1.row]:='Not Agree';
ObjFacRel[stringGrid1.col,stringGrid1.row].SOF:=0.0;
end;


procedure Tform3.Button2Click(Sender: TObject);
   var i,j,k: integer;
  
begin
k:=1;
form5.stringGrid1.Rowcount:=FacCount*ObjCount+1;
form5.stringGrid1.colcount:=7;

 for i:=1 to ObjCount do
  for j:=1 to FacCount do
    begin
       form5.stringGrid1.cells[0,k]:=ObjFacRel[j,i].Obj_Fac;
               k:=k+1;
    end;

form5.visible:=true;
form3.visible:=false;
form4.visible:=false;
end;

procedure Tform3.NumericInputPad1Click(Sender: TObject);
begin
form4.visible:=true;
end;

procedure Tform3.Help1Click(Sender: TObject);
begin
form6.visible:=true;
end;

end.
