unit demofm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, memds, FileUtil, DateTimePicker, Forms, Controls,
  Graphics, Dialogs, DBGrids, DbCtrls, EditBtn, ExtCtrls, StdCtrls, Buttons,
  ComboEx, dblookupcomboboxplus;

type

  { TForm1 }

  TForm1 = class(TForm)
    SetValue10Button: TButton;
    CheckComboBox1: TCheckComboBox;
    ComboBoxEx1: TComboBoxEx;
    DataSource1: TDataSource;
    DateEdit1: TDateEdit;
    DateTimePicker1: TDateTimePicker;
    DBGrid1: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    MemDataset1: TMemDataset;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure SetValue10ButtonClick(Sender: TObject);
  private
    DBLookupCombo : TDBLookupComboBoxPlus;
  private
    procedure InitDataset;
    procedure CreateDBLookupComboboxPlus;
    procedure CreateSimpleEdit;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitDataset;
  CreateDBLookupComboboxPlus;
  CreateSimpleEdit;
end;

procedure TForm1.SetValue10ButtonClick(Sender: TObject);
begin
  DBLookupCombo.KeyValue := 10;
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

  for aCnt := 0 to 1000 do
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

procedure TForm1.CreateDBLookupComboboxPlus;
begin
  DBLookupCombo := TDBLookupComboBoxPlus.Create(Self);

  DBLookupCombo.Left := 10;
  DBLookupCombo.Top := 10;
  DBLookupCombo.Width := 356;
  DBLookupCombo.Height := 27;

  DBLookupCombo.ListSource := DataSource1;
  DBLookupCombo.ListField := 'NAME;DESCRIPTION';
  DBLookupCombo.KeyField := 'ID';

  DBLookupCombo.Parent := Panel1;
  DBLookupCombo.Align := alClient;
end;

procedure TForm1.CreateSimpleEdit;
begin

end;

end.

