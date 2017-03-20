unit dblookupcomboboxplus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LCLStrConsts, DB, DBCtrls, EditBtn, Variants, Controls, Graphics;

type

  { TDBLookupPlus }

  TDBLookupPlus = class(TDBLookup)
  public
    function HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
  end;

  { TDBLookupComboBoxPlus }

  TDBLookupComboBoxPlus = class(TCustomEditButton)
  private
    FDataLink: TFieldDataLink;
    FLookup: TDBLookupPlus;
    procedure UpdateLookup;
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure ActiveChange(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetKeyField: string;
    function GetKeyValue: variant;
    function GetListField: string;
    function GetListSource: TDataSource;
    function GetNullValueKey: TShortCut;
    function GetReadOnly: Boolean;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetKeyField(AValue: string);
    procedure SetKeyValue(AValue: variant);
    procedure SetListField(AValue: string);
    procedure SetListSource(AValue: TDataSource);
    procedure SetNullValueKey(AValue: TShortCut);
    procedure SetReadOnly(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EditingDone; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property KeyValue: variant read GetKeyValue write SetKeyValue;
    property Field: TField read GetField;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditChange; override;

    procedure CloseUp; virtual;
    procedure DropDown; virtual;
    procedure Select; virtual;

    procedure InitializeWnd; override;
    procedure Loaded; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateData(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure WndProc(var Message: TLMessage); override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property KeyField: string read GetKeyField write SetKeyField;
    property ListField: string read GetListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey default 0;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property ButtonOnlyWhenFocused;
    property ButtonCaption;
    property ButtonCursor;
    property ButtonHint;
    property ButtonWidth;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DirectInput;
    property Glyph;
    property NumGlyphs;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Flat;
    property FocusOnButtonClick;
    property Font;
    property Layout;
    property MaxLength;
    property OnButtonClick;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Spacing;
    property Visible;
    property Text;
    property TextHint;
    property TextHintFontColor;
    property TextHintFontStyle;
  end;

implementation

uses
  LCLType;

{ TDBLookupPlus }

function TDBLookupPlus.HandleNullKey(var Key: Word; Shift: TShiftState
  ): Boolean;
var
  i: Integer;
begin
  Result:=False;
  if NullValueKey = KeyToShortCut(Key, Shift) then
  begin
    // null associated field
    if Assigned(ControlLink) and (DataFieldNames<>'') and ControlLink.Active then
    begin
      ControlLink.DataSet.Edit;
      for i:=0 to DataFields.Count-1 do
        TField(DataFields[i]).Clear;
    end else
      Result:=Assigned(FListLink.DataSet) and FListLink.DataSet.Active and Assigned(FListField);
    Key:=VK_UNKNOWN;
  end;
end;

{ TDBLookupComboBoxPlus }

procedure TDBLookupComboBoxPlus.UpdateLookup;
begin
  if ([csLoading, csDestroying] * ComponentState) = [] then
  begin
{ FIXME
    FLookup.Initialize(FDataLink, Items);
    i := FLookup.GetKeyIndex;
    ItemIndex := i;
    if i = -1 then
      Text := '';
}
  end;
end;

procedure TDBLookupComboBoxPlus.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBLookupComboBoxPlus.ActiveChange(Sender: TObject);
begin
  if FDataLink.Active then
    UpdateLookup;
end;

function TDBLookupComboBoxPlus.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBLookupComboBoxPlus.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBLookupComboBoxPlus.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBLookupComboBoxPlus.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TDBLookupComboBoxPlus.GetKeyValue: variant;
begin
  //FIXME Result := FLookup.GetKeyValue(ItemIndex);
  Result := Null;
end;

function TDBLookupComboBoxPlus.GetListField: string;
begin
  Result := FLookup.ListField;
end;

function TDBLookupComboBoxPlus.GetListSource: TDataSource;
begin
  Result := FLookup.ListSource;
end;

function TDBLookupComboBoxPlus.GetNullValueKey: TShortCut;
begin
  Result := FLookup.NullValueKey;
end;

function TDBLookupComboBoxPlus.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBLookupComboBoxPlus.SetDataField(AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TDBLookupComboBoxPlus.SetDataSource(AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBLookupComboBoxPlus.SetKeyField(AValue: string);
begin
  FLookup.KeyField := AValue;
  UpdateLookup;
end;

procedure TDBLookupComboBoxPlus.SetKeyValue(AValue: variant);
begin
//FIXME  ItemIndex := FLookup.GetKeyIndex(AValue);
end;

procedure TDBLookupComboBoxPlus.SetListField(AValue: string);
begin
  FLookup.ListField := AValue;
  UpdateLookup;
end;

procedure TDBLookupComboBoxPlus.SetListSource(AValue: TDataSource);
begin
  FLookup.ListSource := AValue;
  UpdateLookup;
end;

procedure TDBLookupComboBoxPlus.SetNullValueKey(AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

procedure TDBLookupComboBoxPlus.SetReadOnly(AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

constructor TDBLookupComboBoxPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;

  FLookup := TDBLookupPlus.Create(Self);
  FDataLink.OnActiveChange := @ActiveChange;
end;

destructor TDBLookupComboBoxPlus.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

procedure TDBLookupComboBoxPlus.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

function TDBLookupComboBoxPlus.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBLookupComboBoxPlus.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

procedure TDBLookupComboBoxPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TDBLookupComboBoxPlus.EditChange;
begin
  FDataLink.Modified;
  inherited EditChange;
end;

procedure TDBLookupComboBoxPlus.CloseUp;
begin
  FDataLink.UpdateRecord;
end;

procedure TDBLookupComboBoxPlus.DropDown;
begin

end;

procedure TDBLookupComboBoxPlus.InitializeWnd;
begin
  inherited InitializeWnd;
end;

procedure TDBLookupComboBoxPlus.Loaded;
begin
  inherited Loaded;
  UpdateLookup;
end;

procedure TDBLookupComboBoxPlus.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FLookup.HandleNullKey(Key, Shift) then
  begin
    //clear selection
    Text := '';
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TDBLookupComboBoxPlus.UpdateData(Sender: TObject);
var
  i: Integer;
begin
  // combo that has edit control may have unreliable itemindex like bug 20950
  if Style <> csDropDownList then
    ItemIndex := Items.IndexOf(Text);
  i := ItemIndex;
  if i <> -1 then
    FLookup.UpdateData(i, true)
  else
    Text := '';
end;

procedure TDBLookupComboBoxPlus.DataChange(Sender: TObject);
var
  i: Integer;
begin
  if FDatalink.Active then
    i := FLookup.GetKeyIndex
  else
    i := -1;
  ItemIndex := i;
  if i = -1 then
    Text := '';
end;

procedure TDBLookupComboBoxPlus.Select;
begin
  //avoid reseting text when calling select
  FDataLink.OnDataChange := nil;
  try
    if FDataLink.Edit then
    begin
       FDataLink.Modified;
    end
    else
    begin
       // if cannot modify, let it reset
       FDatalink.Reset;
       DataChange(Self);
    end;
  finally
    FDataLink.OnDataChange := @DataChange;
  end;
end;

procedure TDBLookupComboBoxPlus.WndProc(var Message: TLMessage);
begin
  case Message.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      begin
        if FDataLink.CanModify then
        begin
          //LCL changes the Text before LM_PASTE is called and not after like Delphi. Issue 20330
          //When Edit is called the Text property is reset to the previous value
          //Add a workaround while bug is not fixed
          FDataLink.OnDataChange := nil;
          FDatalink.Edit;
          FDataLink.Modified;
          FDataLink.OnDataChange := @DataChange;
          inherited WndProc(Message);
        end;
      end;
    else
      inherited WndProc(Message);
  end;
end;

end.
