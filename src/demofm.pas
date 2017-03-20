unit demofm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, memds, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DateEdit1: TDateEdit;
    DBGrid1: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    MemDataset1: TMemDataset;
    procedure FormCreate(Sender: TObject);
  private
    procedure InitDataset;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  dblookupcomboboxplus;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitDataset;
end;

procedure TForm1.InitDataset;
var
  aCnt : Integer = 0;
begin
  if MemDataset1.Active then
  begin
    MemDataset1.Close;
  end;

  MemDataset1.Open;

  for aCnt := 0 to 100 do
  begin
    MemDataset1.Append;

    MemDataset1.FieldByName('ID').AsLargeInt := aCnt;
    MemDataset1.FieldByName('NAME').AsString := 'Name ' + IntToStr(aCnt);
    MemDataset1.FieldByName('DESCRIPTION').AsString := 'Descrioption ' + IntToStr(aCnt);
    MemDataset1.FieldByName('PRICE').AsCurrency := Random * 100;

    MemDataset1.Post;
  end;

  MemDataset1.First;
end;

end.

