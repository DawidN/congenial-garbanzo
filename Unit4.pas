unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;
  var s:string;
implementation

uses Unit3, fuzzy;

{$R *.DFM}

procedure TForm4.Button1Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.1';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.1;
end;

procedure TForm4.Button2Click(Sender: TObject);

begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.2';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.2;
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.3';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.3;
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.4';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.4;
end;

procedure TForm4.Button5Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.5';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.5;
end;

procedure TForm4.Button6Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.6';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.6;
end;

procedure TForm4.Button7Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.7';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.7;
end;

procedure TForm4.Button8Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.8';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.8;
end;

procedure TForm4.Button9Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.9';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.9;
end;

procedure TForm4.Button11Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='1.0';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*1.0;
end;

procedure TForm4.Button10Click(Sender: TObject);
begin
form3.stringGrid1.Cells[form3.stringGrid1.col,form3.stringGrid1.row]:='0.0';
ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF:=ObjFacRel[form3.stringGrid1.col,form3.stringGrid1.row].SOF*0.0;
end;

end.
