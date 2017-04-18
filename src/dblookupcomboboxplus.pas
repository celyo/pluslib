unit dblookupcomboboxplus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, DB, DBCtrls, memds, EditBtn, Variants, Controls, Graphics, Forms;

type

  { TDBLookupPlus }

  TDBLookupPlus = class(TComponent)
  private
    FControlLink: TFieldDataLink;
    FListLink: TDataLink;
    FListSource: TDataSource;
    FExternalLookupSource: TDataSource;
    FInternalLookupSource: TDataSource;
    FKeyFieldNames: string;
    FListFieldNames: string;
    FListFieldIndex: Integer;
    FKeyFields: TList;   // Key fields in lookup dataset
    FListFields: TList;  // List fields in lookup dataset
    FInitializing: Boolean;
    FHasLookUpField: Boolean;
    FMemDataSet : TMemDataset;
    procedure ActiveChange(Sender: TObject);
    procedure DatasetChange(Sender: TObject);
    function GetKeyFieldName: string;
    function GetListFieldName: string;
    function GetListSource: TDataSource;
    procedure SetKeyFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FetchLookupData;
    procedure DoInitialize;
    procedure InitMemDataSet;
    function SingleListField: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(AControlDataLink: TFieldDataLink);
    function GetKeyFieldValue: Variant;
    function GetListFieldValue: Variant;
    function Locate(AValue : Variant) : Boolean;
    // properties to be published by owner control
    // these are not used where data control Field is dbLookup
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property ListField: string read GetListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property InternalLookupSource: TDataSource read FInternalLookupSource;
  end;

  { TPopupDisplaySettings }

  TPopupDisplaySettings = class(TPersistent)
  private
    FRows: Integer;
    FShowTitles: Boolean;
    FWidth: Integer;
  published
    property Rows : Integer read FRows write FRows;
    property ShowTitles : Boolean read FShowTitles write FShowTitles;
  //  property Sizable : Boolean read FSizable write FSizable;
    property Width : Integer read FWidth write FWidth;
  end;

  TReturnValueEvent = procedure (Sender: TObject; const AValue: Variant) of object;

  { TDBLookupComboBoxPlus }

  TDBLookupComboBoxPlus = class(TCustomEditButton)
  private
    FDataLink: TFieldDataLink;
    FLookup: TDBLookupPlus;
    FNullValueKey: TShortcut;
    FPopupDisplaySettings: TPopupDisplaySettings;
    FPopupForm: TForm;
    FDroppedDown: Boolean;
    procedure UpdateText;
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
    function GetPopupDisplaySettings: TPopupDisplaySettings;
    procedure SetPopupDisplaySettings(AValue: TPopupDisplaySettings);
  protected
    procedure DrawArrowButtonGlyph;
    procedure FocusAndMaybeSelectAll;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditChange; override;
    procedure ButtonClick; override;

    procedure CloseUp; virtual;
    procedure DropDown; virtual;
    procedure Select; virtual;

    procedure InitializeWnd; override;
    procedure Loaded; override;

    procedure EditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateData(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure WndProc(var Message: TLMessage); override;

    function HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
    function HandleDropDownKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure HandlePopupReturnValue(Sender: TObject; const AValue: Variant);
    procedure HandlePopupShowHide(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EditingDone; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property KeyValue: variant read GetKeyValue write SetKeyValue;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property KeyField: string read GetKeyField write SetKeyField;
    property ListField: string read GetListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey default 0;

    property PopupDisplaySettings: TPopupDisplaySettings read GetPopupDisplaySettings write SetPopupDisplaySettings;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property ButtonOnlyWhenFocused;
    property ButtonCaption;
    property ButtonCursor;
    property ButtonHint;
    property ButtonWidth;
    property FocusOnButtonClick;
    property Glyph;
    property NumGlyphs;
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
    property DragMode;
    property EchoMode;
    property Enabled;
    property Flat;
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
  LCLType, DBGrids, Math;

type

{ TDBLookupDataLinkPlus }

TDBLookupDataLinkPlus = class(TDataLink)
private
  FLookup: TDBLookupPlus;
  FRecordUpdated: Boolean;
protected
  procedure ActiveChanged; override;
  procedure DataEvent(Event: DB.TDataEvent; Info: Ptrint); override;
public
  constructor Create(ALookup: TDBLookupPlus);
end;

{ TGridPopupForm }

TGridPopupForm = class(TForm)
private
  FDBGrid : TDBGrid;

  FClosed: boolean;
  FParentRect: TRect;
  FOnReturnValue: TReturnValueEvent;
  FKeyFields : String;
  FListFields : String;
  function GetDataSource: TDataSource;
  function GetValue: Variant;
  procedure SetDataSource(AValue: TDataSource);

  procedure InitializeGridColumns;
  procedure InitializeGrid(ADataSource : TDataSource; AKeyFields : String; AListFields : String);
  procedure Initialize(const ParentRect: TRect; AValue: Variant;
                       const DisplaySettings: TPopupDisplaySettings);
  procedure KeepInView;
  procedure ApplyDisplaySettings(ADisplaySettings: TPopupDisplaySettings);
  procedure ReturnValue;

  procedure GridClick({%H-}Column: TColumn);
  procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormDeactivate(Sender: TObject);
protected
  procedure DoClose(var CloseAction: TCloseAction); override;
  procedure DoCreate; override;
  procedure Deactivate; override;
  function Locate(AValue : Variant) : Boolean;

  property DataSource : TDataSource read GetDataSource write SetDataSource;
  property Value : Variant read GetValue;
  procedure Paint; override;
public
  constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  destructor Destroy; override;
end;

procedure TGridPopupForm.GridClick(Column: TColumn);
begin
  ReturnValue;
end;

procedure TGridPopupForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Handled: Boolean;
begin
  if Shift=[] then
  begin
    Handled := true;
    case Key of
      VK_ESCAPE:
        Close;
      VK_RETURN, VK_SPACE:
          ReturnValue
      else
        Handled := false;
    end;
    if Handled then
      Key := 0;
  end;
end;

procedure TGridPopupForm.FormDeactivate(Sender: TObject);
begin
  Hide;
  if (not FClosed) then
    Close;
end;

procedure TGridPopupForm.Initialize(const ParentRect: TRect; AValue: Variant;
  const DisplaySettings: TPopupDisplaySettings);
begin
  FParentRect := ParentRect;
  ApplyDisplaySettings(DisplaySettings);
  KeepInView;
  Locate(AValue);
end;

function TGridPopupForm.GetValue: Variant;
begin
  if Assigned(FDBGrid.DataSource) and FDBGrid.DataSource.DataSet.Active and (FKeyFields <> '') then
    Result := FDBGrid.DataSource.DataSet.FieldValues[FKeyFields]
  else
    Result := Null;
end;

function TGridPopupForm.GetDataSource: TDataSource;
begin
  Result := FDBGrid.DataSource;
end;

procedure TGridPopupForm.SetDataSource(AValue: TDataSource);
begin
  FDBGrid.DataSource := AValue;
end;

procedure TGridPopupForm.KeepInView;
var
  ABounds: TRect;
  APopupOrigin: TPoint;
begin
  APopupOrigin := Point(FParentRect.Left, FParentRect.Top + FParentRect.Height);
  ABounds := Screen.MonitorFromPoint(APopupOrigin).BoundsRect;
  if APopupOrigin.X + Width > ABounds.Right then
    Left := ABounds.Right - Width
  else
    Left := APopupOrigin.X;
  if APopupOrigin.Y + Height > ABounds.Bottom then
    Top := ABounds.Bottom - Height
  else
    Top := APopupOrigin.Y;
end;

procedure TGridPopupForm.ApplyDisplaySettings(
  ADisplaySettings: TPopupDisplaySettings);
begin
  if (ADisplaySettings.ShowTitles) then
  begin
    FDBGrid.Options := FDBGrid.Options + [dgTitles];
  end
  else
  begin
    FDBGrid.Options := FDBGrid.Options - [dgTitles];
  end;


  if (ADisplaySettings.Width > 0) then
  begin
    Width := ADisplaySettings.Width;
  end
  else
  begin
    Width := FParentRect.Width; // auto size
  end;

  if (ADisplaySettings.Rows > 0) then
  begin
    Height := IfThen(ADisplaySettings.ShowTitles, ADisplaySettings.Rows + 1, ADisplaySettings.Rows) * FDBGrid.DefaultRowHeight;
  end
  else
  begin
    Height := IfThen(ADisplaySettings.ShowTitles, 10 + 1,10) * FDBGrid.DefaultRowHeight; // auto size
  end;
end;

procedure TGridPopupForm.ReturnValue;
begin
  if Assigned(FOnReturnValue) then
    FOnReturnValue(Self, Value);
  if not FClosed then
    Close;
end;

procedure TGridPopupForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  FClosed := true;
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  CloseAction := caFree;
end;

procedure TGridPopupForm.DoCreate;
begin
  FClosed := false;
  Application.AddOnDeactivateHandler(@FormDeactivate);

  inherited DoCreate;
end;

procedure TGridPopupForm.Deactivate;
begin
  inherited Deactivate;
  FormDeactivate(Self);
end;

function TGridPopupForm.Locate(AValue: Variant) : Boolean;
begin
  if Assigned(FDBGrid.DataSource) and FDBGrid.DataSource.DataSet.Active and (FKeyFields <> '') then
  begin
    FDBGrid.DataSource.DataSet.DisableControls;
    try
      Result := FDBGrid.DataSource.DataSet.Locate(FKeyFields, AValue, []);
    finally
      FDBGrid.DataSource.DataSet.EnableControls;
    end;
  end
  else
  begin
    Result := false;
  end;
end;

procedure TGridPopupForm.InitializeGridColumns;
var
  AListFields : TStringList;
  aIndex : Integer;
begin
  AListFields := TStringList.Create;
  FDBGrid.Columns.BeginUpdate;
  try
    FDBGrid.Columns.Clear;

    AListFields.Delimiter := ';';
    AListFields.DelimitedText := FListFields;

    for aIndex := 0 to AListFields.Count - 1 do
    begin
      with FDBGrid.Columns.Add do
      begin
        FieldName := AListFields[aIndex];
        ReadOnly := true;
        Visible := true;
      end;
    end;
  finally
    FDBGrid.Columns.EndUpdate;
    AListFields.Free;
  end;
end;

procedure TGridPopupForm.InitializeGrid(ADataSource: TDataSource;
  AKeyFields: String; AListFields: String);
begin
  DataSource := ADataSource;
  FKeyFields := AKeyFields;
  FListFields := AListFields;

  InitializeGridColumns;
end;

procedure TGridPopupForm.Paint;
begin
  inherited Paint;
  //Canvas.Pen.Color := clWindowText;
  //Canvas.Pen.Style := psSolid;
  //Canvas.Rectangle(0, 0, Width-1, Height-1);
end;

constructor TGridPopupForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);

  BorderStyle := bsNone;
  PopupMode := pmAuto;
  ShowInTaskBar := stNever;

  FDBGrid := TDBGrid.Create(Self);
  FDBGrid.OnCellClick := @GridClick;
  FDBGrid.OnKeyDown := @GridKeyDown;

  FDBGrid.Parent := Self;
  FDBGrid.Align := alClient;
  FDBGrid.ReadOnly := true;
  FDBGrid.AutoEdit := false;
  FDBGrid.AutoFillColumns := true;
  FDBGrid.Options := [dgTitles,dgColumnResize,dgColumnMove,dgColLines,dgRowSelect,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit,dgAnyButtonCanSelect,dgDisableDelete,dgDisableInsert];
end;

destructor TGridPopupForm.Destroy;
begin
  FDBGrid.Free;
  inherited Destroy;
end;



{ TDBLookupDataLinkPlus }

procedure TDBLookupDataLinkPlus.ActiveChanged;
begin
  inherited ActiveChanged;
  FLookup.ActiveChange(Self);
end;

procedure TDBLookupDataLinkPlus.DataEvent(Event: DB.TDataEvent; Info: Ptrint);
begin
  inherited DataEvent(Event, Info);

  if Event = deDataSetChange then
  begin
    if FRecordUpdated then
    begin
      FRecordUpdated := False;
      FLookup.DatasetChange(Self);
    end;
  end
  else if Event = deUpdateRecord then
  begin
    FRecordUpdated := True;
  end;
end;

constructor TDBLookupDataLinkPlus.Create(ALookup: TDBLookupPlus);
begin
  inherited Create;
  FLookup := ALookup;
end;

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
    FetchLookupData;
    if Assigned(FControlLink) and FControlLink.Active then
      FControlLink.Reset;
  end;
end;

procedure TDBLookupPlus.DoInitialize;
var
  LookupResultField: TField;
  ListLinkDataset: TDataSet;
begin
  FKeyFields.Clear;
  FListFields.Clear;
  FHasLookUpField := False;
  if Assigned(FControlLink) and Assigned(FControlLink.DataSet)
    and FControlLink.DataSet.Active then
  begin
    if Assigned(FControlLink.Field) then
    begin
      FHasLookUpField := (FControlLink.Field.FieldKind = fkLookup);
      if FHasLookUpField then
      begin
        if FExternalLookupSource = nil then
          FExternalLookupSource := TDataSource.Create(Self);
        if (FExternalLookupSource.DataSet <> FControlLink.Field.LookupDataSet) then
          FExternalLookupSource.DataSet:= FControlLink.Field.LookupDataSet;
        FListLink.DataSource := FExternalLookupSource;
        FKeyFieldNames := FControlLink.Field.LookupKeyFields;
      end;
    end;
  end;
  if not FHasLookUpField then
    FListLink.DataSource := FListSource;

  if (FKeyFieldNames > '') and FListLink.Active then
  begin
    ListLinkDataset := FListLink.DataSet;
    ListLinkDataset.GetFieldList(FKeyFields, FKeyFieldNames);
    if FHasLookUpField then
    begin
      LookupResultField := ListLinkDataset.FindField(FControlLink.Field.LookupResultField);
      if (Assigned(LookupResultField)) then
      begin
        FListFields.Add(LookupResultField);
      end;
      FListFieldIndex := 0;
    end
    else
    begin
      ListLinkDataset.GetFieldList(FListFields, FListFieldNames);
      if ((FKeyFields.Count > 0) and (FListFields.Count = 0)) then
      begin
        FListFields.AddList(FKeyFields);
      end;
    end;
    InitMemDataSet;
    FetchLookupData;
  end;
end;

procedure TDBLookupPlus.InitMemDataSet;
var
  FieldsToAdd : TList;
  CurrentField : TField;
  AddedFields : String;
  aIndex : Integer;
begin
  if FMemDataSet.Active then
  begin
    FMemDataSet.Close;
  end;
  FMemDataSet.FieldDefs.Clear;

  FieldsToAdd := TList.Create;
  try
    if (FKeyFields.Count > 0) then
    begin
      FieldsToAdd.AddList(FKeyFields);
    end;
    if (FListFields.Count > 0) then
    begin
      FieldsToAdd.AddList(FListFields);
    end;

    AddedFields := '';
    for aIndex := 0 to FieldsToAdd.Count - 1 do
    begin
      CurrentField := TField(FieldsToAdd[aIndex]);
      if Pos(';' + CurrentField.FieldName + ';', AddedFields) <= 0 then
      begin
        with FMemDataSet.FieldDefs.AddFieldDef do
        begin
          Name := CurrentField.FieldName;
          DataType := CurrentField.DataType;
          Size := CurrentField.Size;
          //Precision := CurrentField.Precision;

          if (not CurrentField.Visible) then
          begin
            Attributes := Attributes + [faHiddenCol];
          end;
          if (CurrentField.ReadOnly) then
          begin
            Attributes := Attributes + [faReadonly];
          end;
          if (CurrentField.Required) then
          begin
            Attributes := Attributes + [faRequired];
          end;
        end;

        AddedFields := AddedFields + ';' + CurrentField.FieldName + ';';
      end;
    end;

  finally
    FieldsToAdd.Free;
  end;

  FMemDataSet.FieldDefs.Updated := false;
  FMemDataSet.FieldDefs.Update;
end;

function TDBLookupPlus.SingleListField: String;
var
  AFieldNameList : TStringList;
begin
  Result := '';

  if (FListFieldNames <> '') then
  begin
    AFieldNameList := TStringList.Create;
    try
      AFieldNameList.Delimiter := ';';
      AFieldNameList.DelimitedText := FListFieldNames;

      if (FListFieldIndex >= 0) and (FListFieldIndex < AFieldNameList.Count) then
      begin
        Result := AFieldNameList[FListFieldIndex];
      end;
    finally
      AFieldNameList.Free;
    end;
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

procedure TDBLookupPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent = FListSource) then
    FListSource := nil;
end;

procedure TDBLookupPlus.FetchLookupData;
var
  ListLinkDataSet: TDataSet;
  Bookmark: TBookmark;
begin
  if FMemDataSet.Active then
  begin
    FMemDataSet.Close;
  end;
  FMemDataSet.Open;

  ListLinkDataSet := FListLink.DataSet;
  if (not Assigned(ListLinkDataSet)) or ListLinkDataSet.IsEmpty then
  begin
    Exit;
  end;

  Bookmark := ListLinkDataSet.GetBookmark;
  //ListLinkDataSet.BlockReadSize := 1;
  FMemDataSet.DisableControls;
  try
    ListLinkDataSet.First;
    while not ListLinkDataSet.EOF do
    begin
      FMemDataSet.Append;
      FMemDataSet.FieldValues[FKeyFieldNames] := ListLinkDataSet.FieldValues[FKeyFieldNames];
      FMemDataSet.FieldValues[FListFieldNames] := ListLinkDataSet.FieldValues[FListFieldNames];
      FMemDataSet.Post;

      ListLinkDataSet.Next;
    end;
  finally
    FMemDataSet.First;
    FMemDataSet.EnableControls;
    ListLinkDataSet.GotoBookmark(Bookmark);
    ListLinkDataSet.FreeBookmark(Bookmark);
    //ListLinkDataSet.BlockReadSize := 0;
  end;
end;

constructor TDBLookupPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeyFields := TList.Create;
  FListFields := TList.Create;
  FListLink := TDBLookupDataLinkPlus.Create(Self);
  FMemDataSet := TMemDataset.Create(Self);
  FInternalLookupSource := TDataSource.Create(Self);
  FInternalLookupSource.DataSet := FMemDataSet;
  //FInternalLookupSource.AutoEdit := false;
end;

destructor TDBLookupPlus.Destroy;
begin
  if FMemDataSet.Active then
  begin
    FMemDataSet.Close;
  end;

  FKeyFields.Destroy;
  FListFields.Destroy;
  FListLink.Destroy;
  FInternalLookupSource.Destroy;
  FMemDataSet.Destroy;

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

function TDBLookupPlus.GetKeyFieldValue: Variant;
begin
  if Assigned(FInternalLookupSource) and Assigned(FInternalLookupSource.DataSet) and FInternalLookupSource.DataSet.Active and (FKeyFieldNames <> '') then
  begin
    Result := FInternalLookupSource.DataSet.FieldValues[FKeyFieldNames]
  end
  else
  begin
    Result := Null;
  end;
end;

function TDBLookupPlus.GetListFieldValue: Variant;
begin
  if Assigned(FInternalLookupSource) and Assigned(FInternalLookupSource.DataSet) and FInternalLookupSource.DataSet.Active and (FListFieldNames <> '') then
  begin
    Result := FInternalLookupSource.DataSet.FieldByName(SingleListField).AsVariant;
  end
  else
  begin
    Result := Null;
  end;
end;

function TDBLookupPlus.Locate(AValue: Variant): Boolean;
begin
  if Assigned(FInternalLookupSource) and Assigned(FInternalLookupSource.DataSet) and FInternalLookupSource.DataSet.Active and (FKeyFieldNames <> '') then
  begin
    FInternalLookupSource.DataSet.DisableControls;
    try
      Result := FInternalLookupSource.DataSet.Locate(FKeyFieldNames, AValue, []);
    finally
      FInternalLookupSource.DataSet.EnableControls;
    end;
  end
  else
  begin
    Result := false;
  end;
end;

{ TDBLookupComboBoxPlus }

procedure TDBLookupComboBoxPlus.UpdateText;
var
  AListValue : Variant;
begin
  AListValue := FLookup.GetListFieldValue;
  if not VarIsNull(AListValue) then
  begin
    EditText := AListValue;
  end
  else
  begin
    EditText := '';
  end;
end;

procedure TDBLookupComboBoxPlus.UpdateLookup;
begin
  if ([csLoading, csDestroying] * ComponentState) = [] then
  begin
    FLookup.Initialize(FDataLink);

{ FIXME - this is for debug only }
    //if (FLookup.InternalLookupSource.DataSet.Active) then
    //begin
    //  EditText := IntToStr(FLookup.InternalLookupSource.DataSet.RecordCount);
    //end;
    UpdateText;
  end;
end;

function TDBLookupComboBoxPlus.GetPopupDisplaySettings: TPopupDisplaySettings;
begin
  Result := FPopupDisplaySettings;
end;

procedure TDBLookupComboBoxPlus.SetPopupDisplaySettings(
  AValue: TPopupDisplaySettings);
begin
  FPopupDisplaySettings := AValue;
end;

procedure TDBLookupComboBoxPlus.DrawArrowButtonGlyph;
type
TArrowShape = (asClassicSmaller, asClassicLarger, asModernSmaller,
                                           asModernLarger, asYetAnotherShape);
const
  ArrowColor = TColor($8D665A);
var
  AArrowShape : TArrowShape = asClassicSmaller;
begin
// First I ment to put arrow images in a lrs file. In my opinion, however, that
// wouldn't be an elegant option for so simple shapes.

  if Assigned(Glyph) then
  begin
    Glyph.SetSize(9, 6);
    Glyph.Canvas.Brush.Style := bsSolid;
    Glyph.Canvas.Brush.Color := clSkyBlue;
    Glyph.Canvas.FillRect(0, 0, 9, 6);
    Glyph.Canvas.Pen.Color := ArrowColor;
    Glyph.Canvas.Brush.Color := Glyph.Canvas.Pen.Color;

{ Let's draw shape of the arrow on the button: }
    case AArrowShape of
      asClassicLarger:
        { triangle: }
        Glyph.Canvas.Polygon([Point(0, 1), Point(8, 1),
                                                        Point(4, 5)]);
      asClassicSmaller:
        { triangle -- smaller variant:  }
        Glyph.Canvas.Polygon([Point(1, 2), Point(7, 2),
                                                        Point(4, 5)]);
      asModernLarger:
        { modern: }
        Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0),
                          Point(4, 3), Point(7, 0), Point(8, 1), Point(4, 5)]);
      asModernSmaller:
        { modern -- smaller variant:    }
        Glyph.Canvas.Polygon([Point(1, 2), Point(2, 1),
                          Point(4, 3), Point(6, 1), Point(7, 2), Point(4, 5)]);
      asYetAnotherShape:
        { something in between, not very pretty:  }
        Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0),
              Point(2, 1), Point(6, 1),Point(7, 0), Point(8, 1), Point(4, 5)]);
    end;

    Glyph.Mask(clSkyBlue);
  end;
end;

procedure TDBLookupComboBoxPlus.FocusAndMaybeSelectAll;
begin
  Edit.SetFocus;
  if AutoSelect then
  begin
    Edit.SelectAll;
  end
  else
  begin
    Edit.SelStart := MaxInt;
  end;
end;

procedure TDBLookupComboBoxPlus.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBLookupComboBoxPlus.ActiveChange(Sender: TObject);
begin
  if FDataLink.Active then
  begin
    UpdateLookup;
  end;
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
  Result := FLookup.GetKeyFieldValue;
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
  Result := FNullValueKey;
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
  FLookup.Locate(AValue);
  UpdateText;
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
  FNullValueKey := AValue;
end;

procedure TDBLookupComboBoxPlus.SetReadOnly(AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

constructor TDBLookupComboBoxPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPopupDisplaySettings := TPopupDisplaySettings.Create;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;

  FLookup := TDBLookupPlus.Create(Self);
  FDataLink.OnActiveChange := @ActiveChange;

  FocusOnButtonClick := false;
  DrawArrowButtonGlyph;
end;

destructor TDBLookupComboBoxPlus.Destroy;
begin
  FPopupDisplaySettings.Destroy;
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
    begin
      DataSource := nil;
    end
    else if (AComponent = FPopupForm) then
    begin
      FPopupForm := nil;
    end;
  end;
end;

procedure TDBLookupComboBoxPlus.EditChange;
begin
  FDataLink.Modified;
  inherited EditChange;
end;

procedure TDBLookupComboBoxPlus.ButtonClick;
begin
  inherited ButtonClick;
  DropDown;
  //if FocusOnButtonClick then FocusAndMaybeSelectAll;
end;

procedure TDBLookupComboBoxPlus.CloseUp;
begin
  if not Assigned(FPopupForm) then
  begin
    FPopupForm.Close;
  end;

  FDataLink.UpdateRecord;
end;

procedure TDBLookupComboBoxPlus.DropDown;
var
  AGridPopupForm : TGridPopupForm;
  AControlOrigin : TPoint;
begin
  if not Assigned(FPopupForm) then
  begin
    AGridPopupForm := TGridPopupForm.CreateNew(nil);
    FPopupForm := AGridPopupForm;
  end;


  AControlOrigin := ControlOrigin;
  AGridPopupForm.InitializeGrid(FLookup.InternalLookupSource, FLookup.KeyField, FLookup.ListField);
  AGridPopupForm.Initialize(Rect(AControlOrigin.x, AControlOrigin.Y, AControlOrigin.x + Width, AControlOrigin.y + Height), KeyValue, PopupDisplaySettings);
  AGridPopupForm.FOnReturnValue := @HandlePopupReturnValue;
  AGridPopupForm.OnShow := @HandlePopupShowHide;
  AGridPopupForm.OnHide := @HandlePopupShowHide;
  AGridPopupForm.FreeNotification(Self);

  AGridPopupForm.Show;
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

procedure TDBLookupComboBoxPlus.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  if HandleNullKey(Key, Shift) then
  begin
    // clear selection
    Text := '';
  end
  else if HandleDropDownKey(Key, Shift) then
  begin
    // Do nothing here
  end;

  inherited EditKeyDown(Key, Shift);
end;

procedure TDBLookupComboBoxPlus.UpdateData(Sender: TObject);
var
  i: Integer;
begin
  //FIXME
  // combo that has edit control may have unreliable itemindex like bug 20950
  //if Style <> csDropDownList then
  //  ItemIndex := Items.IndexOf(Text);
  //i := ItemIndex;
  //if i <> -1 then
  //  FLookup.UpdateData(i, true)
  //else
  //  Text := '';
end;

procedure TDBLookupComboBoxPlus.DataChange(Sender: TObject);
begin
  UpdateText;
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
       UpdateText;
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
        if Assigned(FDataLink) and FDataLink.CanModify then
        begin
          //LCL changes the Text before LM_PASTE is called and not after like Delphi. Issue 20330
          //When Edit is called the Text property is reset to the previous value
          //Add a workaround while bug is not fixed
          FDataLink.OnDataChange := nil;
          try
            FDatalink.Edit;
            FDataLink.Modified;
          finally
            FDataLink.OnDataChange := @DataChange;
          end;


          inherited WndProc(Message);
        end;
      end;
    else
      inherited WndProc(Message);
  end;
end;

function TDBLookupComboBoxPlus.HandleNullKey(var Key: Word; Shift: TShiftState
  ): Boolean;
var
  i: Integer;
begin
  Result := false;

  if FNullValueKey = KeyToShortCut(Key, Shift) then
  begin
    if Assigned(FDataLink) and FDataLink.CanModify then
    begin
      FDataLink.DataSet.Edit;
      //FIXME
      //for i:=0 to FDataFields.Count-1 do
      //begin
      //  TField(FDataFields[i]).Clear;
      //end;
    end;
    Key:=VK_UNKNOWN;
  end;
end;

function TDBLookupComboBoxPlus.HandleDropDownKey(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := false;

  if (Key = VK_DOWN) and (Shift = [ssAlt]) then
  begin
    Result := true;
    Key:=VK_UNKNOWN;

    DropDown;
  end;
end;

procedure TDBLookupComboBoxPlus.HandlePopupReturnValue(Sender: TObject;
  const AValue: Variant);
begin
  KeyValue := AValue;
end;

procedure TDBLookupComboBoxPlus.HandlePopupShowHide(Sender: TObject);
begin
  FDroppedDown := (Sender as TForm).Visible;
end;

end.

