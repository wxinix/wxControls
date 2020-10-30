unit wxObjectInspector.Utils;

interface

uses
  System.Classes,
  System.Rtti,
  System.TypInfo,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  WinApi.Windows;

/// <summary>
///   Convert a boolean value to string. The boolean string can be internationalized.
/// </summary>
function BoolToI18nStr(B: Boolean): string;

/// <summary>
///   Test if a child window is contained inside the parent window. <br />
/// </summary>
/// <param name="AChildWnd">
///   The child window.
/// </param>
/// <param name="AParentWnd">
///   The parent window.
/// </param>
function ContainsWindow(AChildWnd, AParentWnd: HWND): Boolean;

/// <summary>
///  Output the string to the DebugView application.
/// </summary>
/// <param name="AString"> (string) The string to output to DebugView.</param>
procedure DebugPrint(const AString: string);

/// <summary>
///   Draw a square button at the specified position on the canvas to show the
///   expand/collapse status. A cross is drawn at the center of the square. The
///   height and width of the cross equal to the square side length minus 4
///   pixel.
/// </summary>
/// <param name="ASideLen">
///   Side length of the square button.
/// </param>
/// <remarks>
///   The square side has a minimum length of 9 pixels.
/// </remarks>
procedure DrawExpandButton(ACanvas: TCanvas; AXPos, AYPos: Integer; ACollapsed: Boolean; ASideLen:
    Integer = 9);

/// <summary>
///   Draw horizontal dotted line.
/// </summary>
procedure DrawHorizontalDottedLine(ACanvas: TCanvas; AXStartPos, AYPos, AXEndPos: Integer);

/// <summary>
///   Check if an element is already included in the Set.
/// </summary>
function ElementInSet(ASet, ASetElement: Integer): Boolean;

/// <summary>
///   Fill a rectangle using the specified color.
/// </summary>
procedure FillRectEx(ADeviceContext: HDC; ARect: TRect; AFillColor: COLORREF);

/// <summary>
///   Fill the rectangle with the specified fill color and border color.
/// </summary>
procedure FillRectWithBorderColor(ADeviceContext: HDC; ARect: TRect;
  AFillColor, ABorderColor: COLORREF; ABorderSize: Integer);

/// <summary>
///   Get the VCL control that has the caret.
/// </summary>
function GetCaretControl: TControl;

/// <summary>
///   Get the handle to the window that is displaying the caret.
/// </summary>
function GetCaretWnd: HWND;

/// <summary>
///   Get the parent form of the specified component.
/// </summary>
function GetComponentParentForm(AComponent: TObject): TCustomForm;

/// <summary>
///   Get value of the specified property from a component. Ony the first
///   property of the same type form the component is returned.
/// </summary>
function GetComponentPropertyValueByType(AType: PTypeInfo; AComponent: TComponent): TValue;

/// <summary>
///   Get event handlers from a form, associated with the specified event. The
///   method name and code address are saved in each item of AItems.
/// </summary>
procedure GetFormEventHandlers(AForm: TCustomForm; AEventType: TRttiType; var AItems: TStrings);

/// <summary>
///   Get the textual name of the method.
/// </summary>
function GetMethodName(AMethod: TValue; AObject: TObject): string;

/// <summary>
///   Get the ordinal value of a Set.
/// </summary>
function GetSetOrdinalValue(const AValue: TValue): Integer;

/// <summary>
///   Check if the component has a property with the specified name.
/// </summary>
function HasComponentProperty(const APropertyName: string;
  AComponentClass: TComponentClass): Boolean;

/// <summary>
///   Check if caret is visible and blinking.
/// </summary>
function IsCaretVisible: Boolean;

/// <summary>
///   Check if a class is derived from another base class.
/// </summary>
function IsClassDerivedFromClass(const AClassType: TClass; ABaseClass: TClass): Boolean;

/// <summary>
///   This function return true if a parent class has a child class that has a
///   property of the parent class type. For example:
///   <code lang="Delphi">TChild = class;
/// TParent = class
/// published
/// property Child:TChild;
/// end;
///
/// TChild = class
/// published
/// property Parent:TParent;
/// end; </code>
/// </summary>
function IsCircularLink(ATypeInfo: Pointer; AQualifiedTypeStr: string; AComponent: TObject;
    AObjectVisibility: TMemberVisibility): Boolean;

/// <summary>
///   Check if a property is derived type of another base class.
/// </summary>
function IsPropTypeDerivedFromClass(const APropType: TRttiType; ABaseClass: TClass): Boolean;

/// <summary>
///   Check the member visibility of the property.
/// </summary>
function IsPropVisible(const AProperty: TRttiProperty; const APropertyOwner: TObject;
  AVisibility: TMemberVisibility): Boolean;

/// <summary>
///   Check if the specified type is TRttiMethodType, and if so, continue to
///   check if it is of the same type as the TRttiMethod.
/// </summary>
function IsSameMethodType(ARttiType: TRttiType; AMethod: TRttiMethod): Boolean;

/// <summary>
///   Check if the value is a signed value.
/// </summary>
function IsSignedValue(const AValue: TValue): Boolean;

/// <summary>
///   Move the subject Rect to the vertical center of the Bound Rect. As a
///   result, the Left position of the subject Rect overlaps with the Left
///   position of the Bound Rect. Their vertical centers are aligned at the
///   same position.
/// </summary>
/// <param name="ARect">
///   The subject Rect.
/// </param>
/// <param name="ABoundRect">
///   The Bound Rect.
/// </param>
procedure MoveRectToBoundVCenter(var ARect: TRect; ABoundRect: TRect);

/// <summary>
///   Check if the object has at least one property visible.
/// </summary>
function ObjectHasAtLeastOneChild(AObject: TObject; AVisibility: TMemberVisibility): Boolean;

/// <summary>
///   Convert a shortcut to string.
/// </summary>
function ShortCutToTextEx(AShortCut: TShortCut): string;

/// <summary>
///   Check if a string starts with the specified substring.
/// </summary>
function StartsWith(ASubText, AText: string; const ACaseSensitive: Boolean = False): Boolean;

/// <summary>
///   Convert a virtual key to its ASCII character if possible.
/// </summary>
function VirtualKeyToStr(AVirtualKey: Word): string;

type
  TValueConverter = class
    /// <summary>
    ///   Convert the value explicity to the specified type.
    /// </summary>
    class function GetValueAs<T>(const AValue: TValue): T;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.Consts,
  Vcl.Styles,
  Vcl.Themes,
  wxObjectInspector.Core;

resourcestring
  SFalse = 'False';
  STrue = 'True';

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp, mkcPgDn, mkcEnd, mkcHome,
    mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns, mkcDel, mkcShift, mkcCtrl, mkcAlt);

const
  MenuKeyCaps: array [TMenuKeyCap] of string = (SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace,
    SmkcPgUp, SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight, SmkcDown, SmkcIns, SmkcDel,
    SmkcShift, SmkcCtrl, SmkcAlt);

function ContainsWindow(AChildWnd, AParentWnd: HWND): Boolean;
var
  parentWnd: HWND;
begin
  Result := (AChildWnd = AParentWnd); // Self-parented.
  if Result then
    Exit;
  parentWnd := GetParent(AChildWnd);

  while parentWnd <> 0 do
  begin
    Result := (parentWnd = AParentWnd);
    if Result then
      Exit;
    parentWnd := GetParent(parentWnd);
  end;
end;

procedure DebugPrint(const AString: string);
begin
  OutputDebugString(PChar(AString));
end;

procedure DrawExpandButton(ACanvas: TCanvas; AXPos, AYPos: Integer; ACollapsed: Boolean; ASideLen:
    Integer = 9);
const
  cInsideMargin  = 2; // 2 pixels
  cMinSideLen    = 9; // 9 pixels
  cOutsideMargin = 2; // 2 pixels
var
  style: TCustomStyleServices;
  themeDetails: TThemedElementDetails;
begin
  // the side length of the square button must be greater than 9 pix
  Assert(ASideLen >= cMinSideLen);
  // Add outside margin for the square button.
  Inc(AXPos, cOutsideMargin);
  Inc(AYPos, cOutsideMargin);
  style := StyleServices;

  if style.Enabled then
  begin
    if ACollapsed then
      themeDetails := style.GetElementDetails(tcbCategoryGlyphClosed)
    else
      themeDetails := style.GetElementDetails(tcbCategoryGlyphOpened);
    // Draw using style
    style.DrawElement(ACanvas.Handle, themeDetails,
      Rect(AXPos, AYPos, AXPos + ASideLen, AYPos + ASideLen));
  end
  else
  begin
    ACanvas.Pen.Color := clBtnShadow;
    ACanvas.brush.Color := clWindow;
    // Draw the rectangle
    ACanvas.Rectangle(AXPos, AYPos, AXPos + ASideLen, AYPos + ASideLen);

    ACanvas.Pen.Color := clWindowText;
    ACanvas.MoveTo(AXPos + cInsideMargin, AYPos + ASideLen div 2);
    // Draw the horizontal line of the cross
    ACanvas.LineTo(AXPos + ASideLen - cInsideMargin, AYPos + ASideLen div 2);

    if ACollapsed then
    begin
      ACanvas.MoveTo(AXPos + ASideLen div 2, AYPos + cInsideMargin);
      // Draw the vertical line of the cross
      ACanvas.LineTo(AXPos + ASideLen div 2, AYPos + ASideLen - cInsideMargin);
    end;
  end;
end;

procedure DrawHorizontalDottedLine(ACanvas: TCanvas; AXStartPos, AYPos, AXEndPos: Integer);
const
  cPixelGap = 2;
var
  pos: Integer;
begin
  for pos := AXStartPos to AXEndPos do
    if pos mod cPixelGap = 0 then
    begin
      ACanvas.MoveTo(pos, AYPos);
      ACanvas.LineTo(pos + 1, AYPos);
    end;
end;

function ElementInSet(ASet, ASetElement: Integer): Boolean;
var
  intSet: TIntegerSet; // TIntegerSet treat an integer as a set of bits.
begin
  intSet := TIntegerSet(ASet);
  Result := ASetElement in intSet;
end;

procedure FillRectEx(ADeviceContext: HDC; ARect: TRect; AFillColor: COLORREF);
var
  brush: HBRUSH;
begin
  if ARect.IsEmpty then
    Exit;
  brush := CreateSolidBrush(AFillColor);
  { The FillRect function fills a rectangle by using the specified brush.
    This function includes the left and top borders, but excludes the right
    and bottom borders of the rectangle.
  }
  FillRect(ADeviceContext, ARect, brush);
  DeleteObject(brush);
end;

procedure FillRectWithBorderColor(ADeviceContext: HDC; ARect: TRect;
  AFillColor, ABorderColor: COLORREF; ABorderSize: Integer);
begin
  { The InflateRect function increases or decreases the width and height of the specified
    rectangle. The InflateRect function adds ABorderSize units to the left and right ends of
    the rectangle and dy units to the top and bottom. The ABorderSize and dy parameters are signed
    values; positive values increase the width and height, and negative values decrease them. }
  FillRectEx(ADeviceContext, ARect, ABorderColor);
  InflateRect(ARect, - ABorderSize, - ABorderSize);
  FillRectEx(ADeviceContext, ARect, AFillColor);
end;

function GetCaretControl: TControl;
begin
  Result := FindControl(GetCaretWnd);
end;

function GetCaretWnd: HWND;
var
  guiThreadInfo: TGUIThreadInfo;
begin
  Result := 0;
  FillChar(guiThreadInfo, sizeof(TGUIThreadInfo), #0);
  guiThreadInfo.cbSize := sizeof(TGUIThreadInfo);

  if GetGUIThreadInfo(GetCurrentThreadId, guiThreadInfo) then
    if guiThreadInfo.hwndCaret > 0 then
      Result := guiThreadInfo.hwndCaret;
end;

function GetComponentParentForm(AComponent: TObject): TCustomForm;
begin
  Result := nil;
  if AComponent is TCustomForm then
    Result := TCustomForm(AComponent)
  else
    if AComponent is TControl then
      Result := GetParentForm(TControl(AComponent));
end;

function GetComponentPropertyValueByType(AType: PTypeInfo; AComponent: TComponent): TValue;
var
  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiPropertyArray: TArray<TRttiProperty>;
  rttiType: TRttiType;
begin
  Result := TValue.Empty;

  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(AComponent.ClassInfo);
  rttiPropertyArray := rttiType.GetProperties;

  for rttiProperty in rttiPropertyArray do
  begin
    // Returns the first property of the specified type.
    if rttiProperty.PropertyType.Handle = AType then
    begin
      Result := rttiProperty.GetValue(AComponent);
      Break;
    end;
  end;
end;

procedure GetFormEventHandlers(AForm: TCustomForm; AEventType: TRttiType; var AItems: TStrings);
var
  rttiContext: TRttiContext;
  rttiType: TRttiType;
  rttiMethodArray: TArray<TRttiMethod>;
  rttiMethod: TRttiMethod;
begin
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(AForm.ClassInfo);
  rttiMethodArray := rttiType.GetDeclaredMethods;

  for rttiMethod in rttiMethodArray do
  begin
    if IsSameMethodType(AEventType, rttiMethod) then
    begin
      AItems.AddObject(rttiMethod.Name, TObject(rttiMethod.CodeAddress));
    end;
  end;
end;

function GetMethodName(AMethod: TValue; AObject: TObject): string;
begin
  Result := EmptyStr;
  if (not Assigned(AObject)) or (AMethod.Kind <> tkMethod) or (AMethod.IsEmpty) then
    Exit;
  Result := AObject.MethodName(TValueData(AMethod).FAsMethod.Code);
end;

function GetSetOrdinalValue(const AValue: TValue): Integer;
var
  data: TValueData;
begin
  Result := 0;
  data := TValueData(AValue);

  case AValue.TypeData.OrdType of
    otSByte:
      Result := data.FAsSByte;
    otUByte:
      Result := data.FAsUByte;
    otSWord:
      Result := data.FAsSWord;
    otUWord:
      Result := data.FAsUWord;
    otSLong:
      Result := data.FAsSLong;
    otULong:
      Result := data.FAsULong;
  end;
end;

function HasComponentProperty(const APropertyName: string;
  AComponentClass: TComponentClass): Boolean;
var
  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiType: TRttiType;
begin
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(AComponentClass);
  rttiProperty := rttiType.GetProperty(APropertyName);
  Result := Assigned(rttiProperty);
end;

function IsCaretVisible: Boolean;
var
  guiThreadInfo: TGUIThreadInfo;
begin
  Result := False;

  FillChar(guiThreadInfo, sizeof(TGUIThreadInfo), #0);
  guiThreadInfo.cbSize := sizeof(TGUIThreadInfo);

  if GetGUIThreadInfo(GetCurrentThreadId, guiThreadInfo) then
    Result := (guiThreadInfo.flags and GUI_CARETBLINKING) = GUI_CARETBLINKING;
end;

function IsClassDerivedFromClass(const AClassType: TClass; ABaseClass: TClass): Boolean;
var
  cls: TClass;
begin
  Result := False;
  cls := AClassType;
  while cls <> nil do
  begin
    if cls = ABaseClass then
      Exit(True);
    cls := cls.ClassParent;
  end;
end;

function IsCircularLink(ATypeInfo: Pointer; AQualifiedTypeStr: string; AComponent: TObject;
    AObjectVisibility: TMemberVisibility): Boolean;
var
  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiPropertyArray: TArray<TRttiProperty>;
  rttiType: TRttiType;
  strAfterCurTypeStr, curTypeStr: string;
  posOfCurTypeStrInQualifiedTypeStr: Integer;
begin
  Result := False;
  rttiContext := rttiContext.Create;
  rttiType := rttiContext.GetType(ATypeInfo);
  curTypeStr := rttiType.Tostring;
  posOfCurTypeStrInQualifiedTypeStr := Pos(curTypeStr, AQualifiedTypeStr); // If not found, returns 0

  strAfterCurTypeStr := Copy(
    AQualifiedTypeStr,
    posOfCurTypeStrInQualifiedTypeStr + Length(curTypeStr), // Start Index
    Length(AQualifiedTypeStr)                               // Count
  );

  { Allow adding others property and even the first circular child property. }
  if strAfterCurTypeStr.IsEmpty then
    Exit(False);

  rttiPropertyArray := TzRttiType(rttiType).GetUsedProperties;

  for rttiProperty in rttiPropertyArray do
  begin
    if IsPropVisible(rttiProperty, AComponent, AObjectVisibility) then
    begin
      if (rttiProperty.PropertyType.TypeKind = tkClass) then
      begin
        curTypeStr := rttiProperty.PropertyType.Tostring;
        if Pos(curTypeStr, strAfterCurTypeStr) > 0 then
          Exit(True);
      end;
    end;
  end;
end;

function IsPropTypeDerivedFromClass(const APropType: TRttiType; ABaseClass: TClass): Boolean;
var
  rttiType: TRttiType;
begin
  Result := False;
  if APropType.TypeKind <> tkClass then
    Exit;

  rttiType := APropType;
  while rttiType <> nil do
  begin
    if rttiType.Handle = PTypeInfo(ABaseClass.ClassInfo) then
      Exit(True);
    rttiType := rttiType.BaseType; // Parent base class .
  end;
end;

function IsPropVisible(const AProperty: TRttiProperty; const APropertyOwner: TObject;
  AVisibility: TMemberVisibility): Boolean;
begin
  if APropertyOwner is TPersistent then
    Result := AProperty.Visibility = mvPublished
  else // if APropertyOwner is TObject then
    Result := AProperty.Visibility >= AVisibility;
end;

function IsSameMethodType(ARttiType: TRttiType; AMethod: TRttiMethod): Boolean;
var
  I: Integer;
  paramListLen: Integer;
  methodParamsList: TArray<TRttiParameter>;
  methodParamTypeStr: string;
  methodParam: TRttiParameter;
  propParamsList: TArray<TRttiParameter>;
  propParamTypeStr: string;
  propParam: TRttiParameter;
  rttiType: TRttiInvokableType;
begin
  Result := True;

  if not (ARttiType is TRttiMethodType) then
    Exit(False);
  { Check if had same method type ( Procedure or function) }
  if TRttiMethodType(ARttiType).MethodKind <> AMethod.MethodKind then
    Exit(False);
  rttiType := TRttiInvokableType(ARttiType);
  { Check if both have return type, or neither has retur type }
  if (AMethod.ReturnType = nil) xor (rttiType.ReturnType = nil) then
    Exit(False);
  { Check if had the same ReturnType if its a function }
  if Assigned(AMethod.ReturnType) and Assigned(rttiType.ReturnType) then
    if (rttiType.ReturnType.Handle <> AMethod.ReturnType.Handle) then
      Exit(False);
  { Check if had same Calling Convention }
  if AMethod.CallingConvention <> rttiType.CallingConvention then
    Exit(False);
  { Check if had same parameters }
  methodParamsList := AMethod.GetParameters;
  propParamsList := rttiType.GetParameters;
  if Length(methodParamsList) <> Length(propParamsList) then
    Exit(False);

  paramListLen := Length(methodParamsList);

  for I := 0 to paramListLen - 1 do
  begin
    propParamTypeStr := EmptyStr;
    methodParamTypeStr := EmptyStr;
    propParam := propParamsList[I];
    methodParam := methodParamsList[I];

    if not SameText(methodParam.Name, propParam.Name) then
      Exit(False);

    if Assigned(propParam.ParamType) then
      propParamTypeStr := propParam.ParamType.ToString;

    if Assigned(methodParam.ParamType) then
      methodParamTypeStr := methodParam.ParamType.ToString;

    if not SameText(propParamTypeStr, methodParamTypeStr) then
      Exit(False);
  end;
end;

function IsSignedValue(const AValue: TValue): Boolean;
begin
  Result := (AValue.TypeData.MinValue < 0) or (AValue.TypeData.MinInt64Value < 0);
end;

procedure MoveRectToBoundVCenter(var ARect: TRect; ABoundRect: TRect);
begin
  // Call OffsetRect to move the upper left corner of the rectangle
  // specified by Rect while maintaining the same width and height.

  // Move the Rect to (0, 0) position.
  OffsetRect(ARect, - ARect.Left, - ARect.Top);
  // Move the Rect downward
  OffsetRect(ARect, 0, (ABoundRect.Height - ARect.Height) div 2);
  // Align the vertical center of Rect and Bound
  OffsetRect(ARect, ABoundRect.Left, ABoundRect.Top);
end;

function ObjectHasAtLeastOneChild(AObject: TObject; AVisibility: TMemberVisibility): Boolean;
var
  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiPropertyList: TArray<TRttiProperty>;
  rttiType: TRttiType;
begin
  Result := False;

  if not Assigned(AObject) then
    Exit;

  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(AObject.ClassInfo);
  rttiPropertyList := TzRttiType(rttiType).GetUsedProperties;

  for rttiProperty in rttiPropertyList do
    if IsPropVisible(rttiProperty, AObject, AVisibility) then
      Exit(True);
end;

function StartsWith(ASubText, AText: string; const ACaseSensitive: Boolean = False): Boolean;
begin
  {
    if ASubString.Length > AString.Length then
    Exit(False);

    if not ACaseSensitive then
    begin
    ASubString := ASubString.ToLower;
    AString := AString.ToLower;
    end;

    Result := CompareMem(Pointer(ASubString), Pointer(AString), ByteLength(ASubString));
    // Result := Pos(ASubString, AString) = 1;  // Slower.
  }
  // Just use the RTL
  if ACaseSensitive then
    Result := StartsStr(ASubText, AText)
  else
    Result := StartsText(ASubText, AText);
end;

function VirtualKeyToStr(AVirtualKey: Word): string;
const
  cBufferSize = 4;
var
  buffer: PChar;
  keyboardState: TKeyboardState;
  scanCode: UINT;
begin
  Result := EmptyStr;

  if GetKeyboardState(keyboardState) then
  begin
    buffer := AllocMem(cBufferSize); // Auto zero-fills
    scanCode := MapVirtualKey(AVirtualKey, MAPVK_VK_TO_VSC);
    if ToAscii(AVirtualKey, scanCode, keyboardState, buffer, 0) > 0 then
      Result := buffer;
    FreeMem(buffer, cBufferSize);
  end;
end;

{ The original ShortCutToText function found in Vcl.Menus has a bug with scCommand !!! }
function ShortCutToTextEx(AShortCut: TShortCut): string;
{$REGION 'GetSpecialName'}
  function GetSpecialName(AShortCut: TShortCut): string;
  var
    scanCode: Integer;
    keyName: array [0 .. 255] of Char;
  begin
    Result := EmptyStr;
    scanCode := MapVirtualKey(LoByte(Word(AShortCut)), MAPVK_VK_TO_VSC) shl sizeof(Word);
    // If there is no translation, the return value is zero
    if scanCode <> 0 then
    begin
      if GetKeyNameText(scanCode, keyName, Length(keyName)) <> 0 then
        Result := keyName
    end;
  end;
{$ENDREGION}
const
  sNone    = '(None)';
  sCmdPlus = 'Cmd+';
var
  name: string;
  key: Byte;
begin
  Result := EmptyStr;

  if AShortCut = scNone then
    Exit(sNone);

  key := LoByte(Word(AShortCut));

  case key of
    vkBack:
      name := MenuKeyCaps[mkcBkSp];
    vkTab:
      name := MenuKeyCaps[mkcTab];
    vkReturn:
      name := MenuKeyCaps[mkcEnter];
    vkEscape:
      name := MenuKeyCaps[mkcEsc];
    vkSpace, vkPrior, vkNext, vkEnd, vkHome, vkLeft, vkUp, vkRight, vkDown:
      name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + key - vkSpace)];
    vkInsert, vkDelete:
      name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + key - vkInsert)];
    vk0 .. vk9:
      name := Chr(key - vk0 + Ord('0'));
    vkA .. vkZ:
      name := Chr(key - vkA + Ord('A'));
    vkNumpad0 .. vkNumpad9:
      name := Chr(key - vkNumpad0 + Ord('0'));
    vkF1 .. vkF24:
      name := 'F' + IntToStr(key - vkF1 + 1);
  else
    name := GetSpecialName(AShortCut);
  end;

  if not name.IsEmpty then
  begin
    if (AShortCut and scShift) <> 0 then
      Result := Result + MenuKeyCaps[mkcShift];
    if (AShortCut and scCtrl) <> 0 then
      Result := Result + MenuKeyCaps[mkcCtrl];
    if (AShortCut and scAlt) <> 0 then
      Result := Result + MenuKeyCaps[mkcAlt];
    { ---> Fix scCommand bug <--- }
    if (AShortCut and scCommand) <> 0 then
      Result := Result + sCmdPlus;

    Result := Result + name;
  end
end;

function BoolToI18nStr(B: Boolean): string;
const
  cBoolStrs: array[Boolean] of string = (SFalse, STrue);
begin
  Result := cBoolStrs[B];
end;

class function TValueConverter.GetValueAs<T>(const AValue: TValue): T;
var
  isSigned: Boolean;
  int64Val: Int64; // Signed AValue !
  int64Result: T absolute int64Val; // Signed Result !
  uint64Val: UInt64; // UnSigned AValue !
  uint64Result: T absolute uint64Val; // UnSigned Result !
begin
  { Just to avoid : E2506 Method of parameterized type declared in interface section must not
    use local symbol 'IsValueSigned' => We can not call IsValueSigned !
  }
  { To Avoid Range check error we must check isSigned }
  isSigned := (AValue.TypeData.MinValue < 0) or (AValue.TypeData.MinInt64Value < 0);

  { Always use 64 bit data !
    => Delphi automatically will cast the result !
  }
  if isSigned then
  begin
    int64Val := (TValueData(AValue).FAsSInt64);
    Result := int64Result;
  end
  else
  begin
    uint64Val := (TValueData(AValue).FAsUInt64);
    Result := uint64Result;
  end;
end;

end.
