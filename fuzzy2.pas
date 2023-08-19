unit fuzzy2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;
  
  LabelRec = array[1..5,1..3] of Tlabel;
  var
  Form2: TForm2;
  LabelGroup : LabelRec;

implementation

uses fuzzy;

{$R *.DFM}


procedure TForm2.Button1Click(Sender: TObject);
begin
form1.visible:=true;
form2.visible:=false;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
 Label1.Caption:= ObjToStr[1].Obj ;
 Label2.Caption:= FloatToStr(ObjToStr[1].Strength) ;
 end;















end.
