program fuzzyprj;

uses
  Forms,
  fuzzy in 'fuzzy.pas' {Form1},
  Unit4 in 'Unit4.pas' {Form4},
  Unit3 in 'Unit3.pas' {form3},
  Unit5 in 'Unit5.pas' {Form5},
  Unit2 in 'Unit2.pas' {Form2},
  about in 'about.pas' {AboutBox},
  Unit6 in 'Unit6.pas' {Form6},
  comp2 in 'comp2.pas' {comp};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(Tform3, form3);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(Tcomp, comp);
  Application.Run;
end.
