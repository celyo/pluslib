unit dblookupcomboboxplus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LCLStrConsts, DB, DBCtrls, EditBtn, Variants, Controls, Graphics;

type

  { TDBLookupPlus }

  TDBLookupPlus = class(TComponent)
  private
    FControlLink: TFieldDataLink;
    FListLink: TDataLink;
    FListSource: TDataSource;
    FLookupSource: TDataSource;
    FDataFieldNames: string;
    FKeyFieldNames: string;
    FListFieldNames: string;
    FListFieldIndex: Integer;
    FDataFields: TList;  // Data Fields to lookup/edit
    FKeyFields: TList;   // Key fields in lookup dataset
    FListFields: TList;  // List fields in lookup dataset
    FNullValueKey: TShortcut;
    FInitializing: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DatasetChange(Sender: TObject);
    procedure DoInitialize;
    function GetKeyFieldName: string;
    function GetListFieldName: string;
    function GetListSource: TDataSource;
    procedure SetKeyFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
  protected
    function HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(AControlDataLink: TFieldDataLink);
    function KeyFieldValue: Variant;
    procedure UpdateData(ValueIndex: Integer; ScrollDataset: Boolean);
    function  GetKeyValue: Variant;
    // properties to be published by owner control
    // these are not used where data control Field is dbLookup
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property ListField: string read GetListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property NullValueKey: TShortcut read FNullValueKey write FNullValueKey;
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

procedure TDBLookupPlus.ActiveChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if FListLink.Active then
    Initialize(FControlLink);
end;

procedure TDBLookupPlus.DatasetChange(Sender: TObject);
begin
  if FListLink.Active and not FListLink.Editing then
  begin
    if Assigned(FControlLink) and FControlLink.Active then
      FControlLink.Reset;
  end;
end;

procedure TDBLookupPlus.DoInitialize;
var
  ListFields: TList;
  ListLinkDataset: TDataSet;
begin
  FDataFields.Clear;
  FKeyFields.Clear;
  FListField := nil;
  FHasLookUpField := False;
  FLookUpFieldIsCached := False;
  if Assigned(FControlLink) and Assigned(FControlLink.DataSet)
    and FControlLink.DataSet.Active then
  begin
    if Assigned(FControlLink.Field) then
    begin
      FHasLookUpField := (FControlLink.Field.FieldKind = fkLookup);
      FLookUpFieldIsCached := (FHasLookupField and FControlLink.Field.LookupCache);
      if FHasLookUpField then
      begin
        if FLookupSource = nil then
          FLookupSource := TDataSource.Create(Self);
        if (FLookupSource.DataSet <> FControlLink.Field.LookupDataSet) then
          FLookupSource.DataSet:= FControlLink.Field.LookupDataSet;
        FListLink.DataSource := FLookupSource;
        FDataFieldNames := FControlLink.Field.KeyFields;
        FKeyFieldNames := FControlLink.Field.LookupKeyFields;
      end else
        FDataFieldNames := FControlLink.Field.FieldName;
      FControlLink.DataSet.GetFieldList(FDataFields, FDataFieldNames);
    end;
  end;
  if not FHasLookUpField then
    FListLink.DataSource := FListSource;

  if (FKeyFieldNames > '') and FListLink.Active then
  begin
    ListLinkDataset := FListLink.DataSet;
    ListFields := TList.Create;
    try
      ListLinkDataset.GetFieldList(ListFields, FListFieldNames);
      ListLinkDataset.GetFieldList(FKeyFields, FKeyFieldNames);
      if FHasLookUpField then
      begin
        FListField := ListLinkDataset.FindField(FControlLink.Field.LookupResultField);
        if (Assigned(FListField) and (ListFields.IndexOf(FListField) < 0)) then
          ListFields.Insert(0, FListField);
        if (ListFields.Count > 0) then
          FListField := TField(ListFields[0]);
      end else
      begin
        if ((FKeyFields.Count > 0) and (ListFields.Count = 0)) then
          ListFields.Add(FKeyFields[0]);
        if ((FListFieldIndex > -1) and (FListFieldIndex < ListFields.Count)) then
          FListField := TField(ListFields[FListFieldIndex])
        else
          FListField := TField(ListFields[0]);
      end;
    finally
      ListFields.Free;
    end;
    FetchLookupData;
  end;
end;

function TDBLookupPlus.GetKeyFieldName: string;
begin
  if FHasLookUpField then
    Result:= ''
  else
    Result := FKeyFieldNames;
end;

function TDBLookupPlus.GetListFieldName: string;
begin
  if FHasLookUpField then
    Result:= ''
  else
    Result := FListFieldNames;
end;

function TDBLookupPlus.GetListSource: TDataSource;
begin
  if FHasLookUpField then
    Result:= nil
  else
    Result:= FListSource;
end;

procedure TDBLookupPlus.SetKeyFieldName(const Value: string);
begin
  FKeyFieldNames := Value;
end;

procedure TDBLookupPlus.SetListFieldName(const Value: string);
begin
  FListFieldNames := Value;
end;

procedure TDBLookupPlus.SetListSource(Value: TDataSource);
begin
  if FListSource = Value then
    Exit;
  if Assigned(FListSource) then
    FListSource.RemoveFreeNotification(Self);
  FListSource:= Value;
  if Assigned(FListSource) then
    FListSource.FreeNotification(Self);
end;

function TDBLookupPlus.HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
var
  i: Integer;
begin
  Result:=False;
  if FNullValueKey=KeyToShortCut(Key, Shift) then
  begin
    // null associated field
    if Assigned(FControlLink) and (FDataFieldNames<>'') and FControlLink.Active then
    begin
      FControlLink.DataSet.Edit;
      for i:=0 to FDataFields.Count-1 do
        TField(FDataFields[i]).Clear;
    end else
      Result:=Assigned(FListLink.DataSet) and FListLink.DataSet.Active and Assigned(FListField);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TDBLookupPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent = FListSource) then
    FListSource := nil;
end;

constructor TDBLookupPlus.Create(AOwner: TComponent);
begin
  inherited;
  FDataFields := TList.Create;
  FKeyFields := TList.Create;
  FListLink := TDBLookupDataLink.Create(Self);
end;

destructor TDBLookupPlus.Destroy;
begin
  FDataFields.Destroy;
  FKeyFields.Destroy;
  FListLink.Destroy;
  inherited Destroy;
end;

procedure TDBLookupPlus.Initialize(AControlDataLink: TFieldDataLink);
begin
  if FInitializing then
    Exit;
  FInitializing := True;
  try
    FControlLink := AControlDataLink;
    DoInitialize;
  finally
    FInitializing := False;
  end;
end;

function TDBLookupPlus.KeyFieldValue: Variant;
begin
  if Assigned(FControlLink) and FControlLink.Active and (FDataFieldNames <> '') then
    Result := FControlLink.DataSet.FieldValues[FDataFieldNames]
  else
    Result := Null;
end;

procedure TDBLookupPlus.UpdateData(ValueIndex: Integer; ScrollDataset: Boolean);
var
  I: Integer;
  Key: Variant;
  SavedEvent: TNotifyEvent;
begin
  if (ValueIndex < 0) or (ValueIndex >= Length(FListKeys)) then
    Exit;
  Key := FListKeys[ValueIndex];
  if ScrollDataset then
    FListLink.DataSet.Locate(FKeyFieldNames, Key, []);
  if Assigned(FControlLink) and FControlLink.Active then
  begin
    if VarSameValue(Key, FControlLink.DataSet.FieldValues[FDataFieldNames]) then
      Exit;
    SavedEvent := FControlLink.OnDataChange;
    FControlLink.OnDataChange := nil;
    FControlLink.Modified;
    FControlLink.Edit;
    FControlLink.OnDataChange := SavedEvent;
    if FDataFields.Count = 1 then
      TField(FDataFields[0]).Value := Key
    else
    begin
      for I := 0 to FDataFields.Count -1 do
        TField(FDataFields[I]).Value := Key[I];
    end;
  end;
end;

function TDBLookupPlus.GetKeyValue: Variant;
begin
  if Assigned(FControlLink) and FControlLink.Active and (FDataFieldNames <> '') then
    Result := FControlLink.DataSet.FieldValues[FDataFieldNames]
  else
    Result := Null;
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

