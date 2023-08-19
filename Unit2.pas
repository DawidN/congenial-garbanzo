unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Menus;

type
  TForm2 = class(TForm)
    StringGrid1: TStringGrid;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    Help1: TMenuItem;
    Exit1: TMenuItem;
    competitivenessAnalysis1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure competitivenessAnalysis1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  
   sigma = array[1..6] of real;
var
  Form2: TForm2;
  sum_wt:sigma;
  Xmarkup:single;
implementation

uses fuzzy, Unit6,comp2;

{$R *.DFM}

procedure TForm2.Button1Click(Sender: TObject);
var  i,j: integer;
s:string;
     n,m,p: real;
begin
m:=0;
n:=0;
p:=0;
  for i:=1 to ObjCount do p:=p+ObjToStr[i].strength;

  for j:=1 to 6 do
   begin
   for i:=1 to ObjCount do
     begin
       sum_wt[j]:=sum_wt[j]+QOMRel[j,i].Str;
     end;
       sum_wt[j]:=sum_wt[j]/p;    //sum div sigma W
       Str(sum_wt[j]:3:2,s);
       StringGrid1.Cells[j,Objcount+1]:=s;
     end;

   for i:=1 to 6 do m:=m+sum_wt[i]*mark_range[i];

   for i:=1 to 6 do n:=n+sum_wt[i];

 Xmarkup:=m/n;
 Str(m/n:3:2,s);
 edit1.text:=s;

end;

procedure TForm2.Exit1Click(Sender: TObject);
begin
Application.terminate;
end;

procedure TForm2.Help1Click(Sender: TObject);
begin
form6.visible:=true;
end;

procedure TForm2.competitivenessAnalysis1Click(Sender: TObject);
begin
comp.visible:=true;
end;

end.
