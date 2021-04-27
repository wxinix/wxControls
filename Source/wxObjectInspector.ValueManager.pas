unit wxObjectInspector.ValueManager;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  wxObjectInspector.Core;

// Dialog Code Constants
type
  DialogActionCode = (dcInit = 0, dcBeforeDestroy = 1, dcShow = 2, dcFinished = 3);

type
  TzObjectInspectorValueManager = class
  const
    DefaultCategoryName: string = 'Miscellaneous';
    DefaultColorRectWidth: Integer = 13;
    DefaultHeaderPropText: string = 'Property';
    DefaultHeaderValueText: string = 'Value';
    DefaultMaximumGutterWidth: Integer = 40;
    DefaultMinimumPlusSignWidth: Integer = 10;
  strict private
    class var FEditors: TDictionary<Pointer, TComponentClass>;
    class var FMaximumGutterWidth: Integer;
    class var FMinimumPlusSignWidth: Integer;
    class var FScaledColorRectWidth: Integer;
  private
    {$REGION 'class constructor and destructor'}
    class constructor Create;
    class destructor Destroy;

    {$ENDREGION}
    {$REGION 'class property gettors and settors'}
    class function Get_MinimumPlusSignWidth: Integer; static;
    class procedure Set_MinimumPlusSignWidth(const AValue: Integer); static;
    class function Get_MaximumGutterWidth: Integer; static;
    class procedure Set_MaximumGutterWidth(const AValue: Integer); static;
    class function Get_ScaledColorRectWidth: Integer; static;
    class procedure Set_ScaledColorRectWidth(const AValue: Integer); static;
    {$ENDREGION}
  public
    /// <summary>
    ///   Callbacks on different dialog code.
    /// </summary>
    class function PerformDialogAction(const AItem: PwxPropertyItem; AEditDialog: TComponent; ACode: DialogActionCode): Integer; virtual;

    /// <summary>
    ///   Get the value returned after editing from the AEditDialog .
    /// </summary>
    class function DialogResultValue(const AItem: PwxPropertyItem; AEditDialog: TComponent): TValue; virtual;

    /// <summary>
    ///   Get customized dialog for current item .
    /// </summary>
    class function GetDialog(const AItem: PwxPropertyItem): TComponentClass; virtual;

    /// <summary>
    ///   Get the value when the user click the ExtraRect .
    /// </summary>
    class function GetExtraRectResultValue(const AItem: PwxPropertyItem): TValue; virtual;

    /// <summary>
    ///   Check if value has an ExtraRect like (Color,Boolean)type .
    /// </summary>
    /// <returns>
    ///   non zero to indicate that value must use an ExtraRect .
    /// </returns>
    class function GetExtraRectWidth(const AItem: PwxPropertyItem): Integer; virtual;

    /// <summary>
    ///   Return a user defined custom ListBox .
    /// </summary>
    class function GetListBoxClass(const AItem: PwxPropertyItem): TComponentClass;

    /// <summary>
    ///   Return ListBox AStrings for the current item. The item must have a
    ///   list. If the item cannot have list, an EItemHasNoList exception will
    ///   be thrown.
    /// </summary>
    /// <exception cref="EItemHasNoList">
    ///   The item is not allowed to have list.
    /// </exception>
    class procedure GetListItems(const AItem: PwxPropertyItem; AStrings: TStrings); virtual;

    /// <summary>
    ///   According to the value type of the property item, return a TValue
    ///   data based on the "untyped" AValue. If the type is TFont, TColor, or
    ///   TIcon, then it is treated as UINT64, i.e., the object pointer treated
    ///   as an unsigned integer.
    /// </summary>
    class function GetValue(const AItem: PwxPropertyItem; const AValue): TValue; virtual;

    /// <summary>
    ///   Return the value type of the property item.
    /// </summary>
    class function GetValueType(const AItem: PwxPropertyItem): PropertyItemValueType; virtual;

    /// <summary>
    ///   Check if the current item can have button .
    /// </summary>
    class function HasButton(const AItem: PwxPropertyItem): Boolean; virtual;

    /// <summary>
    ///   Check if the current item has a customized dialog for editing value.
    /// </summary>
    class function HasDialog(const AItem: PwxPropertyItem): Boolean; virtual;

    /// <summary>
    ///   Check if the current item has a dropdown ListBox associated for editing value.
    /// </summary>
    class function HasListBox(const AItem: PwxPropertyItem): Boolean; virtual;

    /// <summary>
    ///   Register an editor for a specific type.
    /// </summary>
    class procedure RegisterEditor(AType: PTypeInfo; AEditorClass: TComponentClass);

    /// <summary>
    ///   Register an editor for the specified object type.
    /// </summary>
    /// <param name="AObjectClass">
    ///   A class derived from TObject.
    /// </param>
    /// <param name="AEditorClass">
    ///   An editor class. It must be derived from one of the two classes:
    ///   TCommonDialog, or TzCustomObjectInspectorEditDialog.
    /// </param>
    class procedure RegisterObjectEditor(const AObjectClass: TClass; AEditorClass: TComponentClass);

    /// <summary>
    ///   Set the specified value to the property item.
    /// </summary>
    class procedure SetValue(const AItem: PwxPropertyItem; var AValue: TValue); virtual;

    /// <summary>
    ///   Convert the input string to specified type.
    /// </summary>
    class function StrToValue(const AItem: PwxPropertyItem; const AString: string): TValue;

    /// <summary>
    ///   Check if item can assign value that is not listed in ListBox .
    /// </summary>
    class function ValueHasOpenProbabilities(const AItem: PwxPropertyItem): Boolean; virtual;
  public

    /// <summary>
    ///   This is the square rect of the color item. It shows the color, on the
    ///   left side of the textual name of the color.
    /// </summary>
    class property ScaledColorRectWidth: Integer read Get_ScaledColorRectWidth write Set_ScaledColorRectWidth;

    /// <summary>
    ///   Minimum width of the plus sign indicating whether a child list has
    ///   been expanded or not.
    /// </summary>
    class property MinimumPlusSignWidth: Integer read Get_MinimumPlusSignWidth write Set_MinimumPlusSignWidth;

    /// <summary>
    ///   Maximum gutter width.
    /// </summary>
    class property MaximumGutterWidth: Integer read Get_MaximumGutterWidth write Set_MaximumGutterWidth;
  end;

  TzObjectInspectorValueManagerClass = class of TzObjectInspectorValueManager;

var
  ValueManager: TzObjectInspectorValueManagerClass = TzObjectInspectorValueManager;

implementation

uses
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Themes,
  WinApi.Windows,
  wxObjectInspector.EditDialog.Graphic,
  wxObjectInspector.EditDialog.Strings,
  wxObjectInspector.Exceptions,
  wxObjectInspector.Utils;

class constructor TzObjectInspectorValueManager.Create;
begin
  if not Assigned(FEditors) then
    FEditors := TDictionary<Pointer, TComponentClass>.Create;

  FScaledColorRectWidth := DefaultColorRectWidth;
  FMinimumPlusSignWidth := DefaultMinimumPlusSignWidth;
  FMaximumGutterWidth := DefaultMaximumGutterWidth;
end;

class destructor TzObjectInspectorValueManager.Destroy;
begin
  FEditors.Free;
end;

class function TzObjectInspectorValueManager.PerformDialogAction(const AItem: PwxPropertyItem; AEditDialog: TComponent; ACode: DialogActionCode): Integer;
begin
  Result := mrOK;

  case ACode of
    dcInit:
      begin
        if AEditDialog is TwxObjectInspectorDialog then
          TwxObjectInspectorDialog(AEditDialog).PropertyItem := AItem;
      end;
    dcBeforeDestroy:
      { Do nothing for now };
    dcShow:
      begin
        if AEditDialog is TwxObjectInspectorDialog then
          Result := TwxObjectInspectorDialog(AEditDialog).ShowModal;
        if AEditDialog is TCommonDialog then
          Result := TCommonDialog(AEditDialog).Execute.ToInteger;
      end;
    dcFinished:
      { Do nothing for now };
  end;
end;

class function TzObjectInspectorValueManager.DialogResultValue(const AItem: PwxPropertyItem; AEditDialog: TComponent): TValue;
var
  propType: PTypeInfo;
begin
  Result := AItem.Value; // for TzObjectInspectorDialog
  propType := AItem.Value.TypeInfo;

  if AEditDialog is TCommonDialog then
    Result := GetComponentPropertyValueByType(propType, AEditDialog);
end;

class function TzObjectInspectorValueManager.Get_ScaledColorRectWidth: Integer;
begin
  Result := FScaledColorRectWidth;
end;

class function TzObjectInspectorValueManager.GetDialog(const AItem: PwxPropertyItem): TComponentClass;
begin
  Result := nil;

  if FEditors.ContainsKey(AItem.Value.TypeInfo) then
    Result := FEditors[AItem.Value.TypeInfo];
end;

class function TzObjectInspectorValueManager.GetExtraRectResultValue(const AItem: PwxPropertyItem): TValue;
var
  boolVal: Boolean;
  enumSet: TIntegerSet;
  enumValue: Integer;
  tmpValue: TValue;
begin
  Result := AItem.Value; // Initialize.
  tmpValue := AItem.Value;

  if AItem.IsSetElement then
  begin
    Integer(enumSet) := GetSetOrdinalValue(tmpValue);
    enumValue := AItem.SetElementValue;

    if (enumValue in enumSet) then
      boolVal := False // Remove the enum element
    else
      boolVal := True; // Add the enum element

    Result := GetValue(AItem, boolVal);
    Exit;
  end;

  case GetValueType(AItem) of
    vtBool:
      begin
        boolVal := not (TValueConverter.GetValueAs<Boolean>(tmpValue));
        Result := GetValue(AItem, boolVal); // Bool: check -> uncheck; uncheck -> check
      end;
  end;
end;

class function TzObjectInspectorValueManager.GetExtraRectWidth(const AItem: PwxPropertyItem): Integer;
var
  elementSize: TSize;
  themeDetails: TThemedElementDetails;
begin
  Result := 0;
  // Only deal with checkboxes.
  case GetValueType(AItem) of
    vtBool, vtSetElement:
      begin
        themeDetails := StyleServices.GetElementDetails(tbCheckBoxUnCheckedNormal);
        StyleServices.GetElementSize(0, themeDetails, esActual, elementSize);
        Result := elementSize.Width;
      end;
    vtColor:
      Result := ScaledColorRectWidth + 1;
  end;
end;

class function TzObjectInspectorValueManager.GetListBoxClass(const AItem: PwxPropertyItem): TComponentClass;
begin
  // Color List Box excluded.
  if AItem.Value.TypeInfo = TypeInfo(TColor) then
    Exit(TwxColorListBox);

  // Cursor List Box excluded.
  if AItem.Value.TypeInfo = TypeInfo(TCursor) then
    Exit(TwxCursorListBox);

  // ShortcutListBox excluded.
  if AItem.Value.TypeInfo = TypeInfo(TShortCut) then
    Exit(TwxShortcutListBox);

  // Default one.
  Result := TwxObjectInspectorListBox;
end;

class procedure TzObjectInspectorValueManager.GetListItems(const AItem: PwxPropertyItem; AStrings: TStrings);
var
  I: Integer;
  parentForm: TCustomForm;
begin
  if not ValueManager.HasListBox(AItem) then
    raise EItemHasNoListBox(AItem.Name);

  if (AItem.Value.TypeInfo = TypeInfo(TColor)) or (AItem.Value.TypeInfo = TypeInfo(TCursor)) then
    Exit;

  // Get parent form so we can search child components matching the subjec item's type
  parentForm := AItem.AssociatedComponentParentForm;

  // Add matching child components of the parent form to the list. These components are candidate
  // to be assigned to the subject item.
  if AItem.AssociatedProperty.PropertyType.TypeKind = tkClass then
  begin
    if Assigned(parentForm) then
    begin
      for I := 0 to parentForm.ComponentCount - 1 do
      begin
        if IsClassDerivedFromClass(parentForm.Components[I].ClassType, TRttiInstanceType(AItem.AssociatedProperty.PropertyType).MetaclassType) then
          AStrings.AddObject(parentForm.Components[I].Name, TObject(parentForm.Components[I]));
      end;
    end;

    Exit;
  end;

  // Add event handlers
  if AItem.AssociatedProperty.PropertyType.TypeKind = tkMethod then
  begin
    if Assigned(parentForm) then
      GetFormEventHandlers(parentForm, AItem.AssociatedProperty.PropertyType, AStrings);

    Exit;
  end;

  // Add True/False
  if AItem.IsSetElement then
  begin
    AStrings.AddObject(BoolToI18nStr(False), TObject(0));
    AStrings.AddObject(BoolToI18nStr(True),  TObject(1));
    Exit;
  end;

  // Add enumerated. Integer type is already filtered out because of HasList. So it must be
  // Enumerated type.
  if AItem.AssociatedProperty.PropertyType.IsOrdinal then
  begin
    for I := AItem.Value.TypeData.MinValue to AItem.Value.TypeData.MaxValue do
      AStrings.AddObject(GetEnumName(AItem.Value.TypeInfo, I), TObject(I));
    Exit;
  end;
end;

class function TzObjectInspectorValueManager.Get_MaximumGutterWidth: Integer;
begin
  Result := FMaximumGutterWidth;
end;

class function TzObjectInspectorValueManager.Get_MinimumPlusSignWidth: Integer;
begin
  Result := FMinimumPlusSignWidth;
end;

class function TzObjectInspectorValueManager.GetValue(const AItem: PwxPropertyItem; const AValue): TValue;
var
  intSet: TIntegerSet;
begin
  Result := AItem.Value; // So as to get the typeinfo. IMPORTANT!!

  case GetValueType(AItem) of
    vtMethod:
      begin
        TValueData(Result).FAsMethod := TMethod(AValue);
        Exit;
      end;
    vtObject:
      begin
        TValueData(Result).FAsObject := TObject(AValue);
        Exit;
      end;
    vtString:
      begin
        Result := string(AValue);
        Exit;
      end;
    vtChar:
      begin
        TValueData(Result).FAsUByte := Byte(AValue);
        Exit;
      end;
    vtSingle:
      begin
        TValueData(Result).FAsSingle := Single(AValue);
        Exit;
      end;
    vtDouble:
      begin
        TValueData(Result).FAsDouble := Double(AValue);
        Exit;
      end;
    vtExtended:
      begin
        TValueData(Result).FAsExtended := Extended(AValue);
        Exit;
      end;
  end;

  if AItem.IsSetElement then
  begin
    Integer(intSet) := GetSetOrdinalValue(AItem.Value);

    if Boolean(AValue) then
      Include(intSet, AItem.SetElementValue)
    else
      Exclude(intSet, AItem.SetElementValue);

    TValueData(Result).FAsSLong := Integer(intSet);
  end
  else
  begin
    if IsSignedValue(AItem.Value) then
      TValueData(Result).FAsSInt64 := Int64(AValue) // If it is a Font, Icon, or Color, return
    else // the pointer!
      TValueData(Result).FAsUInt64 := UInt64(AValue);
  end;
end;

class function TzObjectInspectorValueManager.GetValueType(const AItem: PwxPropertyItem): PropertyItemValueType;
begin
  if AItem.Value.TypeInfo = nil then
    Exit(vtUnknown);

  if Assigned(AItem.AssociatedProperty) then
  begin
    case AItem.AssociatedProperty.PropertyType.TypeKind of
      tkMethod:
        Exit(vtMethod);
      tkString, tkWString, tkUString:
        Exit(vtString);
      tkWChar, tkChar:
        Exit(vtChar);
    end;
  end;

  if AItem.Value.TypeInfo = TypeInfo(TColor) then
    Exit(vtColor);

  if AItem.Value.TypeInfo = TypeInfo(TCursor) then
    Exit(vtCursor);

  if AItem.Value.TypeInfo = TypeInfo(TShortCut) then
    Exit(vtShortCut);

  if AItem.Value.TypeInfo = TypeInfo(TFont) then
    Exit(vtFont);

  if AItem.Value.TypeInfo = TypeInfo(TIcon) then
    Exit(vtIcon);
  if AItem.Value.TypeInfo = TypeInfo(Boolean) then
    Exit(vtBool);

  if AItem.Value.TypeInfo = TypeInfo(Bool) then
    Exit(vtBool);

  if AItem.Value.TypeInfo = TypeInfo(Single) then
    Exit(vtSingle);

  if AItem.Value.TypeInfo = TypeInfo(Double) then
    Exit(vtDouble);

  if AItem.Value.TypeInfo = TypeInfo(Extended) then
    Exit(vtExtended);

  if AItem.IsEnum then
    Exit(vtEnum);

  if AItem.IsSetElement then
    Exit(vtSetElement);

  if AItem.IsSet then
    Exit(vtSet);

  if AItem.IsClass then
    Exit(vtObject);

  Result := vtUnknown;
end;

class function TzObjectInspectorValueManager.HasButton(const AItem: PwxPropertyItem): Boolean;
begin
  Result := False;

  if HasListBox(AItem) or HasDialog(AItem) then
    Result := True;
end;

class function TzObjectInspectorValueManager.HasDialog(const AItem: PwxPropertyItem): Boolean;
var
  rttiType: TRttiType;
  rttiContext: TRttiContext;
  editorClass: TComponentClass;
begin
  editorClass := nil;

  Result := FEditors.ContainsKey(AItem.Value.TypeInfo);

  if not Result then
  begin
    rttiContext := TRttiContext.Create;
    rttiType := rttiContext.GetType(AItem.Value.TypeInfo);
    rttiType := rttiType.BaseType;

    while ((not Result) and Assigned(rttiType)) do begin
      Result := FEditors.ContainsKey(rttiType.Handle.TypeData.ClassType.ClassInfo);

      if Result then
        editorClass := FEditors[rttiType.Handle.TypeData.ClassType.ClassInfo];

      rttiType := rttiType.BaseType;
    end;
  end;

  if Result and (not FEditors.ContainsKey(AItem.Value.TypeInfo)) then
    FEditors.Add(AItem.Value.TypeInfo, editorClass);
end;

class function TzObjectInspectorValueManager.HasListBox(const AItem: PwxPropertyItem): Boolean;
begin
  case GetValueType(AItem) of
    // The property item is a component type, meaning it has events, and we need to
    // list the events in the list box, hence requring a list.
    vtObject:
      Result := IsPropTypeDerivedFromClass(AItem.AssociatedProperty.PropertyType, TComponent);
    vtMethod, vtBool, vtColor, vtCursor, vtShortCut, vtSetElement, vtEnum:
      Result := True;
  else
    Result := False;
  end
end;

class procedure TzObjectInspectorValueManager.RegisterEditor(AType: PTypeInfo;
  AEditorClass: TComponentClass);
begin
  if not Assigned(FEditors) then
    FEditors := TDictionary<Pointer, TComponentClass>.Create;

  if Assigned(AType) and Assigned(AEditorClass) then
    FEditors.Add(AType, AEditorClass);
end;

class procedure TzObjectInspectorValueManager.RegisterObjectEditor(const AObjectClass: TClass; AEditorClass: TComponentClass);
begin
  if not Assigned(FEditors) then
    FEditors := TDictionary<Pointer, TComponentClass>.Create;

  if Assigned(AObjectClass) and Assigned(AEditorClass) then
    FEditors.Add(AObjectClass.ClassInfo, AEditorClass);
end;

class procedure TzObjectInspectorValueManager.Set_MaximumGutterWidth(const AValue: Integer);
begin
  FMaximumGutterWidth := AValue;
end;

class procedure TzObjectInspectorValueManager.Set_MinimumPlusSignWidth(const AValue: Integer);
begin
  FMinimumPlusSignWidth := AValue;
end;

class procedure TzObjectInspectorValueManager.Set_ScaledColorRectWidth(const AValue: Integer);
begin
  FScaledColorRectWidth := AValue;
end;

class procedure TzObjectInspectorValueManager.SetValue(const AItem: PwxPropertyItem; var AValue: TValue);
begin
  AItem^.AssociatedProperty.SetValue(AItem.Instance, AValue);
end;

class function TzObjectInspectorValueManager.StrToValue(const AItem: PwxPropertyItem; const AString: string): TValue;
var
  doubleVal: Double;
  errCode: Integer;
  extendedVal: Extended;
  int64Val: Int64;
  singleVal: Single;
  uint64Val: UInt64;
begin
  Result := TValue.Empty;

  case GetValueType(AItem) of
    vtString:
      begin
        Result := AString;
      end;
    vtChar:
      begin { IsOrdinal = True , but TryStrToInt64 will fail ! }
        if AString <> '' then
          Result := Ord(AString[1]);
      end;
    vtSingle:
      begin
        if TryStrToFloat(AString, singleVal) then
          Result := singleVal;
      end;
    vtDouble:
      begin
        if TryStrToFloat(AString, doubleVal) then
          Result := doubleVal;
      end;
    vtExtended:
      begin
        if TryStrToFloat(AString, extendedVal) then
          Result := extendedVal;
      end;
  else
    if AItem.AssociatedProperty.PropertyType.IsOrdinal then begin
      if IsSignedValue(AItem.Value) then begin
        Val(AString, int64Val, errCode);
        if errCode = 0 then
          Result := int64Val;
      end else begin
        Val(AString, uint64Val, errCode);
        if errCode = 0 then
          Result := uint64Val;
      end;
    end;
  end;
end;

class function TzObjectInspectorValueManager.ValueHasOpenProbabilities(const AItem: PwxPropertyItem): Boolean;
begin
  case GetValueType(AItem) of
    vtColor, vtString, vtUnknown:
      Result := True;
  else
    Result := False;
  end;
end;

initialization
  TzObjectInspectorValueManager.RegisterObjectEditor(TStrings, TzStringsEditDialog);
  TzObjectInspectorValueManager.RegisterObjectEditor(TGraphic, TzGraphicEditDialog);
  TzObjectInspectorValueManager.RegisterObjectEditor(TFont, TFontDialog);
  TzObjectInspectorValueManager.RegisterEditor(TypeInfo(TColor), TColorDialog);
end.
