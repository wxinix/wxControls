unit wxObjectInspector.Core;

interface

uses
  Generics.Collections,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  WinApi.Messages,
  WinApi.Windows;

type

  /// <summary>
  ///   Providing format preference for floating point numbers.
  /// </summary>
  /// <remarks>
  ///   This utility class is used in TwxPropertyItem.ValueAsString, which
  ///   internally calls FloatToStrF to convert a floating point number Value
  ///   into a displayable string, formatting via the <i>Format, Precision, and
  ///   Digits</i> values. <br /><br />The Format parameter is TFloatFormat
  ///   (SysUtils) type, with the following values:
  ///   <list type="bullet">
  ///     <item>
  ///       ffCurrency eg : ?2,345.60
  ///     </item>
  ///     <item>
  ///       ffExponent eg : 2.3456E+04
  ///     </item>
  ///     <item>
  ///       ffFixed eg : 2345.60
  ///     </item>
  ///     <item>
  ///       <b>ffGeneral</b> eg : 2345.6
  ///     </item>
  ///     <item>
  ///       ffNumber eg : 2,345.6
  ///     </item>
  ///   </list>
  ///   TwxPropertyItem.ValueAsString internally calls FLoatToStrF(ffGeneral,
  ///   TwxFloatPreference.ExpPrecision, TwxFloatPreference.MaxDigits).
  /// </remarks>
  /// <seealso cref="TwxPropertyItem.ValueAsString" />
  /// <seealso cref="SysUtils.TFloatFormat" />
  TwxFloatPreference = class(TPersistent)
  strict private
    FExpPrecision: Integer;
    FMaxDigits: Integer;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>
    ///   Exponent precision, default 2, for the subject floating point
    ///   number. When the exponent part is not relevant, the value is treated
    ///   as precision of the number.
    /// </summary>
    property ExpPrecision: Integer read FExpPrecision write FExpPrecision;
    /// <summary>
    ///   Maximum number of digits for the subject floating point number, default 6.
    /// </summary>
    property MaxDigits: Integer read FMaxDigits write FMaxDigits;
  end;

  /// <summary>
  ///   This class is an extension of TStack. Whenever a Canvas is pushed on
  ///   stack, the associated Pen, Font, and Brush objects are pushed in an
  ///   internal stack. When popping up the Canvas, the associated saved Pen,
  ///   Font and Brush objects.
  /// </summary>
  TwxCanvasStack = class(TStack<TCanvas>)
    {$REGION 'CanvasObjectsStore'}
  private type
    ICanvasObjectsStore = interface
      procedure RestoreCanvasObjects(ACanvas: TCanvas);
      procedure SaveCanvasObjects(ACanvas: TCanvas);
    end;

    TCanvasObjectsStore = class(TInterfacedObject, ICanvasObjectsStore)
    strict private
      FBrush: TBrush;
      FFont: TFont;
      FPen: TPen;
      procedure RestoreCanvasObjects(ACanvas: TCanvas);
      procedure SaveCanvasObjects(ACanvas: TCanvas);
    public
      constructor Create;
      destructor Destroy; override;
    end;
    {$ENDREGION}
  strict private
    FObjectsStack: TStack<ICanvasObjectsStore>;
    {$REGION 'Constructor & Destructor'}
  public
    constructor Create;
    destructor Destroy; override;
    {$ENDREGION}
  public
    /// <summary>
    ///   Clear stack.
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Remove top stack item. Extract is the same as Pop except for the
    ///   notification event code indicating an element was extracted rather than removed.
    /// </summary>
    function Extract: TCanvas;
    /// <summary>
    ///   Look at top stack item.
    /// </summary>
    function Peek: TCanvas; overload;
    /// <summary>
    ///   Pop stack item.
    /// </summary>
    function Pop: TCanvas;
    /// <summary>
    ///   Push stack item.
    /// </summary>
    procedure Push(const ACanvas: TCanvas);
    /// <summary>
    ///   Set capacity same as current number of items.
    /// </summary>
    procedure TrimExcess;
  end;

  TwxRttiType = class(TRttiType)
    /// <summary>
    ///   Get a list of properties that are declared by the owner object
    ///   itself, plus properties from its parent object that have distinct
    ///   names. If the property declared by its parent objects uses a name
    ///   that the owner object already uses, it will not be included.
    /// </summary>
    function GetUsedProperties: TArray<TRttiProperty>;
  end;

  /// <summary>
  ///   A pair of Object and its name.
  /// </summary>
  TwxObjectNamePair = TPair<TObject, string>;

  /// <summary>
  ///   A container of object name pairs. Key = Object, Value = Name. The life
  ///   cycle of the contained objects are NOT managed by this class.
  /// </summary>
  TwxObjectHost = class
  private
    FList: TList<TwxObjectNamePair>;
    function GetCount: Integer;
    /// <exception cref="EArgumentOutOfRangeException">
    ///   (AIndex &lt; 0) or (AIndex &gt;= Count)
    /// </exception>
    function GetItem(AIndex: Integer): TwxObjectNamePair;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>
    ///   Add object to the container. Object will be saved as Key, and Name
    ///   will be saved as value.
    /// </summary>
    procedure AddObject(AObject: TObject; const AName: string);
    /// <summary>
    ///   Total number of items in the container.
    /// </summary>
    property Count: Integer read GetCount;
    /// <summary>
    ///   Retrieve item by index. If index is out of bound, an exception will
    ///   be thrown.
    /// </summary>
    property Item[AIndex: Integer]: TwxObjectNamePair read GetItem;
  end;

type
  PropertyItemValueType = (
    vtUnknown = 0,
    vtEnum = 1,
    vtSet = 2,
    vtSetElement = 3,
    vtObject = 4,
    vtMethod = 5,
    vtBool = 6,
    vtString = 7,
    vtChar = 8,
    vtColor = 9,
    vtCursor = 10,
    vtFont = 11,
    vtIcon = 12,
    vtShortCut = 13,
    vtSingle = 14,
    vtDouble = 15,
    vtExtended = 16
  );

type
  PwxPropertyItem = ^TwxPropertyItem;
  TwxPropertyItemList = class;

  TwxPropertyItem = record
  strict private
    FAssociatedProperty: TRttiProperty;
    FCategoryIndex: Integer;
    FCategoryName: string;
    FComponent: TObject;
    FFloatPreference: TwxFloatPreference;
    FInstance: TObject;
    FIsCategory: Boolean;
    FObjectVisibility: TMemberVisibility;
    FOwnerList: TwxPropertyItemList;
    FParent: PwxPropertyItem;
    FQualifiedName: string;
    FSetElementValue: Integer;
    FVisible: Boolean;
    {$REGION 'Property gettors and settors'}
    function Get_AssociatedComponentParentForm: TCustomForm;
    function Get_AssociatedProperty: TRttiProperty;
    procedure Set_AssociatedProperty(const AValue: TRttiProperty);
    function Get_CategoryIndex: Integer;
    procedure Set_CategoryIndex(const AValue: Integer);
    function Get_CategoryName: string;
    procedure Set_CategoryName(const AValue: string);
    function Get_ChildCount: Integer;
    function Get_ChildItems(const AIndex: Integer): PwxPropertyItem;
    function Get_Component: TObject;
    procedure Set_Component(const AValue: TObject);
    function Get_Expanded: Boolean;
    procedure Set_FloatPreference(const AValue: TwxFloatPreference);
    function Get_HasChild: Boolean;
    function Get_Instance: TObject;
    procedure Set_Instance(const AValue: TObject);
    function Get_IsCategory: Boolean;
    procedure Set_IsCategory(const AValue: Boolean);
    function Get_IsClass: Boolean;
    function Get_IsComponent: Boolean;
    function Get_IsEmpty: Boolean;
    function Get_IsEnum: Boolean;
    function Get_IsSet: Boolean;
    function Get_IsSetElement: Boolean;
    function Get_MayHaveChild: Boolean;
    function Get_Name: string;
    function Get_ObjectVisibility: TMemberVisibility;
    procedure Set_ObjectVisibility(const AValue: TMemberVisibility);
    procedure Set_OwnerList(const AValue: TwxPropertyItemList);
    function Get_Parent: PwxPropertyItem;
    procedure Set_Parent(const AValue: PwxPropertyItem);
    function Get_QualifiedName: string;
    procedure Set_QualifiedName(const AValue: string);
    function Get_SetElementValue: Integer;
    procedure Set_SetElementValue(const AValue: Integer);
    function Get_Value: TValue;
    function Get_ValueAsString: string;
    function Get_Visible: Boolean;
    procedure Set_Visible(const AValue: Boolean);
    {$ENDREGION}
  private
    /// <summary>
    ///   Checks if the OwnerList list has been defined. If not, an
    ///   EItemListNotAssigned exception will be thrown. <br />
    /// </summary>
    /// <exception cref="EItemListNotAssigned">
    ///   OwnerList list is nil.
    /// </exception>
    procedure CheckOwnerList;
  public
    /// <summary>
    ///   This zero-out the memory. All members will be reset to zero.
    /// </summary>
    class function Empty: TwxPropertyItem; static;

    /// <summary>
    ///   Checks whether two items are equal.
    /// </summary>
    /// <param name="AItemToCompare">
    ///   Item to compare.
    /// </param>
    function EqualTo(AItemToCompare: PwxPropertyItem): Boolean;

  public
    /// <summary>
    ///   If the component being inspected is a TComponent, this provides its
    ///   parent form. This is useful for getting event handlers defined as
    ///   members of the parent form.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property AssociatedComponentParentForm: TCustomForm read Get_AssociatedComponentParentForm;

    /// <summary>
    ///   This is the associated RTTI property of the item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property AssociatedProperty: TRttiProperty read Get_AssociatedProperty write Set_AssociatedProperty;

    /// <summary>
    ///   If the current item is a category item, this provides the index of
    ///   the subject category in the category list maintained by the
    ///   inspector.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property CategoryIndex: Integer read Get_CategoryIndex write Set_CategoryIndex;

    /// <summary>
    ///   If the current item is a category item, this provides the name of the
    ///   subject category.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property CategoryName: string read Get_CategoryName write Set_CategoryName;

    /// <summary>
    ///   Get the total count of child items.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property ChildCount: Integer read Get_ChildCount;

    /// <summary>
    ///   Get the child item by index. If the index is out of bound, nil will
    ///   be returned.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property ChildItems[const AIndex: Integer]: PwxPropertyItem read Get_ChildItems;

    /// <summary>
    ///   The owner component being inspected. It can be a TObject, or a
    ///   TComponent.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Component: TObject read Get_Component write Set_Component;

    /// <summary>
    ///   The subject item has child items, and is expanded.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property Expanded: Boolean read Get_Expanded;

    /// <summary>
    ///   Floating point preference for presenting floating point numbers. It
    ///   is taken from the inspector. The life cycle is managed by the owning
    ///   inspector, not the subject item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property FloatPreference: TwxFloatPreference write Set_FloatPreference;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property HasChild: Boolean read Get_HasChild;

    /// <summary>
    ///   The instance of the property variable associated with this item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Instance: TObject read Get_Instance write Set_Instance;

    /// <summary>
    ///   Whether this item is a category.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property IsCategory: Boolean read Get_IsCategory write Set_IsCategory;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsClass: Boolean read Get_IsClass;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsComponent: Boolean read Get_IsComponent;

    /// <summary>
    ///   Check current item is empty or not. Empty means all members are
    ///   zeroed-out.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsEmpty: Boolean read Get_IsEmpty;

    /// <summary>
    ///   Check if current item is enumeration type.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsEnum: Boolean read Get_IsEnum;

    /// <summary>
    ///   Check if current item is a set.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsSet: Boolean read Get_IsSet;

    /// <summary>
    ///   Check if current element is a set element.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsSetElement: Boolean read Get_IsSetElement;

    /// <summary>
    ///   Whether the item may have child. Class, Set and Category can have
    ///   children.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property MayHaveChild: Boolean read Get_MayHaveChild;

    /// <summary>
    ///   The name of the property associated with the item.
    /// </summary>
    /// <remarks>
    ///   Read Only.
    /// </remarks>
    property Name: string read Get_Name;

    /// <summary>
    ///   Object visibility value inherited from the object inspector. If the
    ///   object being inspected is a TComponent, then only published members
    ///   are visible. if the object being inspected is a TObject, then the
    ///   visibility specified here dictates. Properties must have the same or
    ///   higher visibility than ObjectVisibility specified here in order to be
    ///   visible to the inspector.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property ObjectVisibility: TMemberVisibility read Get_ObjectVisibility write Set_ObjectVisibility;

    /// <summary>
    ///   OwnerList of the item, i.e., the master item list managed by the inspector.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property OwnerList: TwxPropertyItemList read FOwnerList write Set_OwnerList;

    /// <summary>
    ///   Parent item of the current item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Parent: PwxPropertyItem read Get_Parent write Set_Parent;

    /// <summary>
    ///   Qualified name of the item. This may be prefixed with a category name, or suffixed with
    ///   an index number, seperated by a dot.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property QualifiedName: string read Get_QualifiedName write Set_QualifiedName;

    /// <summary>
    ///   If the subject item is an element of a set, SetElementValue reflects
    ///   the associated ordinal value of the element.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property SetElementValue: Integer read Get_SetElementValue write Set_SetElementValue;

    /// <summary>
    ///   Get the value of this item, based on the RTTI type, and the
    ///   associated instance.
    /// </summary>
    /// <remarks>
    ///   Read Only.
    /// </remarks>
    property Value: TValue read Get_Value;

    /// <summary>
    ///   String representation of the item's value. If current item is an
    ///   element of a set, its value string will be "True" or "False",
    ///   depending on whether the element has its value defined in the set
    ///   variable. Color value string will be the name of the color, while
    ///   cursor value string will be the name of the cursor. If the current
    ///   item is an object, its value string will be the object's name or
    ///   component name string. Method (event handler) will use the method's
    ///   declared name as the value string. Single/Double/Extended will the
    ///   formatted string converted from the numerical value. Everything
    ///   else's value string will be a direct convert from the value. <br />
    /// </summary>
    /// <remarks>
    ///   Read Only.
    /// </remarks>
    property ValueAsString: string read Get_ValueAsString;

    /// <summary>
    ///   Whether the item is visible.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Visible: Boolean read Get_Visible write Set_Visible;
  end;

 /// <summary>
  ///   <para>
  ///     This class provides a generic implementation of record list,
  ///     allowing us to store any record type in a list. The record item <br />
  ///     can be retrieved via pointer, rather than value copy.
  ///   </para>
  ///   <para>
  ///     T must be a record type, while PT must be a record pointer type of T.
  ///   </para>
  /// </summary>
  TwxRecordList<T, PT> = class(TObject)
  private
    type
      /// <summary>
      ///   A type definition for the pointer type of T. It is needed so we can
      ///   play all the trick to allow the generics capability of
      ///   TwxRecordList&lt;T,PT&gt;.
      /// </summary>
      PtrT = ^T;
  strict private
    /// <summary>
    ///   Internal list of record pointers. The generic type PT is converted to general pointer
    ///   before saving to the list.
    /// </summary>
    FRecPtrList: TList;
    function Get_Item(const AIndex: Integer): PT;
    function Get_Count: Integer;
  strict protected
    /// <summary>
    ///   Convert PT to a general pointer. This is needed to enable the generic
    ///   capability.
    /// </summary>
    function PTToPointer(ARecord: PT): Pointer;

    /// <summary>
    ///   Convert a general pointer to PT. This is needed to enable generic
    ///   capability.
    /// </summary>
    function PointerToPT(APointer: Pointer): PT;

    /// <summary>
    ///   This is to release the dynamic memory of the specified record.
    /// </summary>
    /// <param name="APointer">
    ///   The record pointer. We use a general pointer rather then the generic
    ///   parameterized type PT, because the internal list stores as general
    ///   pointer. The internal list cannot store as PT. Also we cannot use
    ///   TList&lt;PT&gt;, because PT is not a known type and we cannot
    ///   instantiate TList&lt;PT&gt;.
    /// </param>
    procedure FreeRecord(APointer: Pointer); virtual;
  public
    /// <summary>
    ///   This is to validate the parameterized types T and PT are indeed
    ///   record type, and record pointer type.
    /// </summary>
    class procedure ValidateTypeParameters;
    {$REGION 'Constructor & Destructor'}
    constructor Create; virtual;
    destructor Destroy; override;
    {$ENDREGION}

  public
    /// <summary>
    ///   Add a record to the list. This function does a deep copy of the
    ///   provided record value.
    /// </summary>
    /// <param name="ARecordValue">
    ///   A record value to be added to the list.
    /// </param>
    /// <returns>
    ///   The index of the newly added record value in the list.
    /// </returns>
    function Add(const ARecordValue: T): Integer; overload; virtual;

    /// <summary>
    ///   Create a new record from heap memory, and add the new record to the
    ///   list, then returns the pointer of the newly created record. The newly
    ///   created record's data members will not initialized so a user needs to
    ///   initialize each data member individually afterwards.
    /// </summary>
    /// <returns>
    ///   Pointer of the newly created record.
    /// </returns>
    function Add: PT; overload; virtual;

    /// <summary>
    ///   Returns the first record pointer in the list.
    /// </summary>
    function First: PT;

    /// <summary>
    ///   Get the index for the specified record pointer. If item is not in the
    ///   list, IndexOf returns -1. If a pointer appears more than once in the
    ///   array, IndexOf returns the index of the first appearance.
    /// </summary>
    function IndexOf(ARecordPtr: PT): Integer;

    /// <summary>
    ///   Get the last record pointer in the list.
    /// </summary>
    function Last: PT;

    /// <summary>
    ///   Clear the list. The heap memory allocated to each individual record
    ///   will be released.
    /// </summary>
    procedure Clear; virtual;

    /// <summary>
    ///   Delete a record by the specified index. If the index is out of bound,
    ///   an exception will be thrown. The record will be removed from the
    ///   list, and released from heap memory.
    /// </summary>
    /// <param name="AIndex">
    ///   Index of the record to be deleted.
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   (AIndex &lt; 0) or (AIndex &gt;= Count)
    /// </exception>
    procedure Delete(const AIndex: Integer);

    /// <summary>
    ///   Sort the list based on the supplied comparer.
    /// </summary>
    /// <param name="ACompare">
    ///   The comparer.
    /// </param>
    procedure Sort(ACompare: TListSortCompare);

  public
    /// <summary>
    ///   Retrieve individual item by index. If the index is out of bound, an
    ///   exception will be thrown.
    /// </summary>
    /// <param name="AIndex">
    ///   Index of the recrod to retrieve.
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   (AIndex &lt; 0) or (AIndex &gt;= Count)
    /// </exception>
    property Items[const AIndex: Integer]: PT read Get_Item;

    /// <summary>
    ///   Total number of items in the list.
    /// </summary>
    property Count: Integer read Get_Count;
  end;

  TwxPropertyItemList = class(TwxRecordList<TwxPropertyItem, PwxPropertyItem>)
    /// <summary>
    ///   Get the index of the item matching specified qualified name. Only
    ///   the first instance is returned. If not found, returns -1.
    /// </summary>
    function IndexOfQualifiedName(AQualifiedName: string): Integer;

    /// <summary>
    ///   Sort the list based on the qualified name of each item.
    /// </summary>
    procedure Sort;
  end;

type
  TObjectInspectorHeaderPart = (hpLeft, hpRight);

type
  TzHeaderMouseDownEvent = procedure(
    Sender: TControl;
    APart: TObjectInspectorHeaderPart;
    X, Y: Integer
  ) of object;

  TzPropertyItemEvent = function(
    Sender: TControl;
    AItem: PwxPropertyItem
  ): Boolean of object;

  TzPropertyItemGetFriendlyNameEvent = function(
    Sender: TControl;
    AItem: PwxPropertyItem
  ): string of object;

  TzPropertyItemSetValueEvent = function(
    Sender: TControl;
    AItem: PwxPropertyItem;
    var ANewValue: TValue
  ): Boolean of object;

  TzSplitterPosChangedEvent = procedure(
    Sender: TControl;
    var APos: Integer
  ) of object;

type
  TwxCustomControl = class(TCustomControl)
  private
    FIsMouseLButtonDown: Boolean;
    {$REGION 'Property Gettors: IzControl Interface'}
    function Get_IsMouseDown: Boolean;
    function Get_IsMouseInControl: Boolean;
    function Get_IsVclStyleUsed: Boolean;
    function Get_UseStyleBorder: Boolean;
    function Get_UseStyleColor: Boolean;
    function Get_UseStyleFont: Boolean;
    {$ENDREGION}
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  public
    property IsMouseDown: Boolean read Get_IsMouseDown;
    property IsMouseInControl: Boolean read Get_IsMouseInControl;
    property IsVclStyleUsed: Boolean read Get_IsVclStyleUsed;
    property UseStyleBorder: Boolean read Get_UseStyleBorder;
    property UseStyleColor: Boolean read Get_UseStyleColor;
    property UseStyleFont: Boolean read Get_UseStyleFont;
  end;

  TwxAbstractObjectInspector = class abstract(TwxCustomControl)
  strict private
    FBorderStyle: TBorderStyle;
    FCanvasStack: TwxCanvasStack;
    FCategories: TList<string>;
    FCircularLinkedProperties: TList<string>;
    FComponent: TObject;
    FComponentClassType: TClass;
    FContext: TRttiContext;
    FDefaultCategoryName: string;
    FDefaultPropertyValueMap: TDictionary<string, string>;
    FExpandedList: TList<string>;
    FFloatPreference: TwxFloatPreference;
    FIsSettingComponent: Boolean;
    FItemHeight: Integer;
    FItems: TwxPropertyItemList;
    FLockUpdate: Boolean;
    FObjectVisibility: TMemberVisibility;
    FOnAutoExpandItemOnInit: TzPropertyItemEvent;
    FOnBeforeAddItem: TzPropertyItemEvent;
    FPropertyCategoryMap: TDictionary<string, Integer>;
    FPropertyInstances: TDictionary<string, TObject>;
    FReadOnly: Boolean;
    FRttiType: TRttiType;
    FSaveVisibleItems: TList<string>;
    FSortByCategory: Boolean;
    FVisibleItems: TList<PwxPropertyItem>;
    {$REGION 'Property gettors and settors'}
    procedure Set_BorderStyle(const Value: TBorderStyle);
    procedure Set_Component(AValue: TObject);
    function Get_FloatPreference: TwxFloatPreference;
    procedure Set_ObjectVisibility(const AValue: TMemberVisibility);
    procedure Set_SortByCategory(const AValue: Boolean);
    {$ENDREGION}
  strict protected
    function CircularLinkedProperties: TList<string>;
    function ExpandedList: TList<string>;
    function ItemOrder(PItem: PwxPropertyItem): Integer;
    function LockUpdate: Boolean;
    function SaveVisibleItems: TList<string>;
    function VisibleItems: TList<PwxPropertyItem>;
    function IsItemCircularLink(AItem: PwxPropertyItem): Boolean;
    function IsValueNotDefault(AQualifiedName: string; AValue: string): Boolean;
  protected
    procedure Changed; virtual;
    procedure ComponentChanged; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateItems;
    procedure UpdateVisibleItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ClearRegisteredCategorys;
    procedure RegisterPropertyInCategory(const ACategoryName, APropertyName: string);

    procedure Invalidate; override;


    function ItemNeedUpdate(AItem: PwxPropertyItem): Boolean;
    function NeedUpdate: Boolean;
    procedure UpdateProperties(const ADoRepaint: Boolean = False); virtual; abstract;

    property BorderStyle: TBorderStyle read FBorderStyle write Set_BorderStyle;
    property CanvasStack: TwxCanvasStack read FCanvasStack;
    property Categories: TList<string> read FCategories;
    property Component: TObject read FComponent write Set_Component;
    property ComponentClassType: TClass read FComponentClassType;
    property DefaultCategoryName: string read FDefaultCategoryName write FDefaultCategoryName;
    property DefaultPropertyValue: TDictionary<string, string> read FDefaultPropertyValueMap;
    property FloatPreference: TwxFloatPreference read Get_FloatPreference;

    property ItemHeight: Integer read FItemHeight write FItemHeight;

    property Items: TwxPropertyItemList read FItems;
    property ObjectVisibility: TMemberVisibility read FObjectVisibility write Set_ObjectVisibility default mvPublic;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property SortByCategory: Boolean read FSortByCategory write Set_SortByCategory;

    property OnAutoExpandItemOnInit: TzPropertyItemEvent read FOnAutoExpandItemOnInit write FOnAutoExpandItemOnInit;
    property OnBeforeAddItem: TzPropertyItemEvent read FOnBeforeAddItem write FOnBeforeAddItem;
  end;

  TwxSplitteredObjectInspector = class abstract(TwxAbstractObjectInspector)
  strict private
    FFixedSplitter: Boolean;
    FOnSplitterPosChanged: TzSplitterPosChangedEvent;
    FSplitterColor: TColor;
    FSplitterDown: Boolean;
    FSplitterPos: Integer;
    {$REGION 'Property gettors and settors'}
    procedure Set_SplitterColor(const AValue: TColor);
    procedure Set_SplitterPos(const AValue: Integer);
    function Get_SplitterRect: TRect;
    {$ENDREGION}
  private
    procedure WMMouseMove(var AMessage: TWMMouseMove); message WM_MouseMove;
  protected
    procedure DrawSplitter(ACanvas: TCanvas); virtual;
    procedure InvalidateNC;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SplitterPosChanged(var ANewPos: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FixedSplitter: Boolean read FFixedSplitter write FFixedSplitter;
    property SplitterColor: TColor read FSplitterColor write Set_SplitterColor;
    property SplitterPos: Integer read FSplitterPos write Set_SplitterPos;
    property SplitterRect: TRect read Get_SplitterRect;
    property OnSplitterPosChanged: TzSplitterPosChangedEvent read FOnSplitterPosChanged write FOnSplitterPosChanged;
  end;

  TwxHeaderedObjectInspector = class abstract(TwxSplitteredObjectInspector)
  strict private
    FHeaderPressed: Boolean;
    FHeaderPropPressed: Boolean;
    FHeaderPropText: string;
    FHeaderValuePressed: Boolean;
    FHeaderValueText: string;
    FOnHeaderMouseDown: TzHeaderMouseDownEvent;
    FShowHeader: Boolean;
    {$REGION 'Property gettors and settors'}
    function Get_HeaderPropRect: TRect;
    procedure Set_HeaderPropText(const AValue: string);
    function Get_HeaderRect: TRect;
    function Get_HeaderValueRect: TRect;
    procedure Set_HeaderValueText(const AValue: string);
    procedure Set_ShowHeader(const AValue: Boolean);
    {$ENDREGION}
  private
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure Paint; override;
    procedure PaintHeader; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property HeaderPropRect: TRect read Get_HeaderPropRect;
    property HeaderPropText: string read FHeaderPropText write Set_HeaderPropText;
    property HeaderRect: TRect read Get_HeaderRect;
    property HeaderValueRect: TRect read Get_HeaderValueRect;
    property HeaderValueText: string read FHeaderValueText write Set_HeaderValueText;
    property ShowHeader: Boolean read FShowHeader write Set_ShowHeader;
    property OnHeaderMouseDown: TzHeaderMouseDownEvent read FOnHeaderMouseDown write FOnHeaderMouseDown;
  end;

  TwxScrollableObjectInspector = class abstract(TwxHeaderedObjectInspector)
  strict private
    FPrevScrollPos: Integer;
    FScrollInfo: TScrollInfo;
    {$REGION 'Property gettors and settors'}
    function Get_VisiblePropCount: Integer;
    {$ENDREGION}
  private
    procedure CMFontChanged(var AMessage: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var AMessage: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var AMessage: TWMSize); message WM_SIZE;
    procedure WMVScroll(var AMessage: TWMVScroll); message WM_VSCROLL;
    procedure WMWindowPosChanged(var AMessage: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  strict protected
    function FirstItemIndex: Integer;
    function IndexFromPoint(APoint: TPoint): Integer;
    function IndexToVirtualIndex(AIndex: Integer): Integer;
    function ItemTop(AIndex: Integer): Integer;
    function LastItemIndex: Integer;
    function MaxItemCount: Integer;
    procedure CreateParams(var AParams: TCreateParams); override;
  protected
    procedure Paint; override;
    procedure PaintBackground(ACanvas: TCanvas); virtual;
    procedure PaintItem(AIndex: Integer); virtual;
    procedure UpdateScrollBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property VisiblePropCount: Integer read Get_VisiblePropCount;
  end;

{$REGION 'Object Inspector Edit Controls'}
type
  TwxObjectInspectorButton = class(TwxCustomControl)
  const
    DefaultArrowSize = 3;
    DefaultWidth = 17;
  strict private
    FDropDown: Boolean;
  class var
    FScaledArrowSize: Integer;
    FScaledWidth: Integer;
    class constructor Create;
    class procedure Set_ScaledArrowSize(const AValue: Integer); static;
    class procedure Set_ScaledWidth(const AValue: Integer); static;
  private
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

    property DropDown: Boolean read FDropDown write FDropDown;
    property OnMouseDown;

    class property ScaledArrowSize: Integer read FScaledArrowSize write Set_ScaledArrowSize;
    class property ScaledWidth: Integer read FScaledWidth write Set_ScaledWidth;
  end;

  TwxObjectInspectorListBox = class(TCustomListBox)
  private
    FItem: Pointer;
    FItemEdit: TCustomEdit;
    procedure Set_Item(const AValue: Pointer);
    procedure Set_ItemEdit(const AValue: TCustomEdit);
  protected
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
  public
    property Item: Pointer read FItem write Set_Item;
    property ItemEdit: TCustomEdit read FItemEdit write Set_ItemEdit;
    property ItemHeight;
    property OnMouseDown;
  end;

  TzObjectInspectorListBoxClass = class of TwxObjectInspectorListBox;

  { =>This class is used to test if the ListBox has custom items.
    =>Any CustomListBox must be derived from this class ! }
  TwxObjectInspectorCustomListBox = class abstract(TwxObjectInspectorListBox)
  protected
    procedure CreateWnd; override;
    procedure PopulateList; virtual; abstract;
  end;

  /// <summary>
  ///   Provides a list box filled with color items, each item has a sample
  ///   rectangle showing the color and the name of the color.
  /// </summary>
  TwxColorListBox = class(TwxObjectInspectorCustomListBox)
  private
    procedure ColorCallBack(const AName: string);
    function GetColor(AIndex: Integer): TColor;
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState); override;
    procedure PopulateList; override; final;
  public
    constructor Create(AOwner: TComponent); override;
    property Colors[AIndex: Integer]: TColor read GetColor;
  end;

  /// <summary>
  ///   Provides a list box filled with a list of cursors.
  /// </summary>
  TwxCursorListBox = class(TwxObjectInspectorCustomListBox)
  private
    procedure CursorCallBack(const AName: string);
    function Get_Cursor(AIndex: Integer): TCursor;
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState); override;
    procedure PopulateList; override; final;
  public
    constructor Create(AOwner: TComponent); override;
    property Cursors[AIndex: Integer]: TCursor read Get_Cursor;
  end;

  /// <summary>
  ///   Provides a list box with a list of predefined short cuts.
  /// </summary>
  TwxShortcutListBox = class(TwxObjectInspectorCustomListBox)
  private
    procedure EnumShortCuts;
    function Get_ShortCut(AIndex: Integer): TShortCut;
  protected
    procedure PopulateList; override; final;
  public
    constructor Create(AOwner: TComponent); override;
    property ShortCuts[AIndex: Integer]: TShortCut read Get_ShortCut;
  end;

  TwxObjectInspectorEdit = class(TCustomEdit)
  strict private
    FButton: TwxObjectInspectorButton;
    FDefaultSelectedIndex: Integer;
    FOwnerInspector: TwxCustomControl;
    FListBox: TwxObjectInspectorListBox;
    FPropertyItem: PwxPropertyItem;
    FTextChanged: Boolean;
    {$REGION 'Gettors and Settors'}
    function Get_ListBox: TwxObjectInspectorListBox;
    function Get_PropertyItem: PwxPropertyItem;
    procedure Set_PropertyItem(const AItem: PwxPropertyItem);
    {$ENDREGION}
  private
    procedure CMCancelMode(var AMessage: TCMCancelMode); message CM_CANCELMODE;
    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;
    procedure WMChar(var AMessage: TWMChar); message WM_CHAR;
    procedure WMKeyDown(var AMessage: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var AMessage: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var AMessage: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSetFocus(var AMessage: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanged(var AMessage: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    /// <remarks>
    ///   WndProc to filter out WM_ACTIVATE message.
    /// </remarks>
    procedure WndProc(var AMessage: TMessage); override;
    /// <remarks>
    ///   Event handler for ButtonClick. It was set inside Create.
    /// </remarks>
    procedure ButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    /// <remarks>
    ///   Set inside InitList
    /// </remarks>
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    /// <remarks>
    ///   Fired inside the settor of PropertyItem.
    /// </remarks>
    procedure PropertyItemChanged;
    {$REGION 'Utility Functions for Message and Event Handlers'}
    /// <summary>
    ///   Hide the list.
    /// </summary>
    /// <remarks>
    ///   Called inside ButtonClick
    /// </remarks>
    procedure HideList;
    /// <summary>
    ///   Show the list.
    /// </summary>
    /// <remarks>
    ///   Called inside ButtonClick
    /// </remarks>
    procedure ShowList;
    /// <summary>
    ///   Called inside WMLButtonDblClk, and WMLButtonDown.
    /// </summary>
    procedure DoDblClick;
    /// <summary>
    ///   Called from inside SetValueFromEdit, WMChar, WMKillFocus
    /// </summary>
    procedure DoSetValueFromEdit;
    /// <summary>
    ///   Called inside ListBoxMouseDown
    /// </summary>
    procedure DoSetValueFromList;
    /// <summary>
    ///   Called inside PropertyItemChanged.
    /// </summary>
    procedure InitList;
    /// <summary>
    ///   Called inside ButtonClick, and OnDblClick
    /// </summary>
    procedure ShowModalDialog;
    /// <summary>
    ///   Called inside WMWindowPosChanged
    /// </summary>
    procedure UpdateButton;
    {$ENDREGION}
  public
    constructor Create(AOwnerInspector: TwxCustomControl); reintroduce;
    procedure SetValueFromEdit;
    /// <summary>
    ///   Update the edit text with the property item's value string.
    /// </summary>
    procedure UpdateEditText;
  public
    {$REGION 'Properties Inherited from TCustomEdit'}
    property AlignWithMargins;
    property BorderStyle;
    property Cursor;
    property CustomHint;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Left;
    property Margins;
    property Name;
    property ParentCustomHint;
    property TabStop;
    property Tag;
    property Top;
    property Width;
    {$ENDREGION}
    property ListBox: TwxObjectInspectorListBox read Get_ListBox;
    property PropertyItem: PwxPropertyItem read Get_PropertyItem write Set_PropertyItem;
  end;
{$ENDREGION}

  TwxCustomObjectInspector = class(TwxScrollableObjectInspector)
  strict private
    FAllowSearch: Boolean;
    FAutoCompleteText: Boolean;
    FBoldHint: Boolean;
    FBoldNonDefaultValue: Boolean;
    FCategoryColor: TColor;
    FCategoryTextColor: TColor;
    FClickTime: Integer;
    FExtraRectIndex: Integer;
    FGridColor: TColor;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FGutterWidth: Integer;
    FHighlightColor: TColor;
    FHintPoint: TPoint;
    FIsItemHint: Boolean;
    FNameColor: TColor;
    FNonDefaultValueColor: TColor;
    FOnCollapseItem: TzPropertyItemEvent;
    FOnExpandItem: TzPropertyItemEvent;
    FOnGetItemFriendlyName: TzPropertyItemGetFriendlyNameEvent;
    FOnGetItemReadOnly: TzPropertyItemEvent;
    FOnItemSetValue: TzPropertyItemSetValueEvent;
    FOnSelectItem: TzPropertyItemEvent;
    FPrevHintIndex: Integer;
    FPropInspEdit: TwxObjectInspectorEdit;
    FPropsNeedHint: Boolean;
    FReadOnlyColor: TColor;
    FReferencesColor: TColor;
    FSearchText: string;
    FSelectedIndex: Integer;
    FSelItem: TwxPropertyItem;
    FSepTxtDis: Integer;
    FShowGridLines: Boolean;
    FShowGutter: Boolean;
    FShowItemHint: Boolean;
    FSubPropertiesColor: TColor;
    FTrackChange: Boolean;
    FUnRegisterKeys: Boolean;
    FValueColor: TColor;
    FValuesNeedHint: Boolean;
    {$REGION 'Property gettors and settors'}
    procedure Set_AllowSearch(const Value: Boolean);
    procedure Set_BoldNonDefaultValue(const Value: Boolean);
    function Get_ExtraRect(Index: Integer): TRect;
    procedure Set_GridColor(const Value: TColor);
    procedure Set_GutterColor(const Value: TColor);
    procedure Set_GutterEdgeColor(const Value: TColor);
    procedure Set_GutterWidth(const Value: Integer);
    procedure Set_HighlightColor(const Value: TColor);
    function Get_ItemRect(Index: Integer): TRect;
    procedure Set_NameColor(const Value: TColor);
    procedure Set_NonDefaultValueColor(const Value: TColor);
    function Get_PlusMinBtnRect(Index: Integer): TRect;
    function Get_PropTextRect(Index: Integer): TRect;
    procedure Set_ReadOnlyColor(const Value: TColor);
    procedure Set_ReferencesColor(const Value: TColor);
    function Get_SelectedItem: PwxPropertyItem;
    procedure Set_ShowGridLines(const Value: Boolean);
    procedure Set_ShowGutter(const Value: Boolean);
    procedure Set_SubPropertiesColor(const Value: TColor);
    procedure Set_ValueColor(const Value: TColor);
    function Get_ValueRect(Index: Integer): TRect;
    function Get_ValueTextRect(Index: Integer): TRect;
    {$ENDREGION}
  private
    procedure CMHintShow(var AMessage: TCMHintShow); message CM_HINTSHOW;
    procedure CMStyleChanged(var AMessage: TMessage); message CM_STYLECHANGED;
    procedure WMHotKey(var AMessage: TWMHotKey); message WM_HOTKEY;
    procedure WMKillFocus(var AMessage: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var AMessage: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetFocus(var AMessage: TWMSetFocus); message WM_SETFOCUS;
  protected
    function CanDrawChevron(Index: Integer): Boolean;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;

    procedure CreateWnd; override;

    function DoCollapseItem(APropItem: PwxPropertyItem): Boolean;
    function DoExpandItem(APropItem: PwxPropertyItem): Boolean;
    procedure DoExtraRectClick;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    function DoSelectCaret(AIndex: Integer): Boolean;
    function DoSetValue(APropItem: PwxPropertyItem; var Value: TValue): Boolean;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    procedure PaintCategory(AIndex: Integer); virtual;
    procedure PaintItem(AIndex: Integer); override;
    procedure PaintItemValue(APropItem: PwxPropertyItem; AIndex: Integer); virtual;

    procedure SplitterPosChanged(var APos: Integer); override;

    procedure UpdateEditControl(const AIsSetValue: Boolean = True);
    procedure UpdateSelIndex;

    procedure WndProc(var AMessage: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CollapseAll;
    function CollapseItem(APropItem: PwxPropertyItem): Boolean;

    procedure ExpandAll;
    function ExpandItem(APropItem: PwxPropertyItem): Boolean;

    procedure RegisterKeys;
    procedure UnRegisterKeys;

    procedure SelectItem(AIndex: Integer);
    function SetPropValue(APropItem: PwxPropertyItem; var Value: TValue): Boolean;

    procedure UpdateProperties(const Repaint: Boolean = False); override;

    property AllowSearch: Boolean read FAllowSearch write Set_AllowSearch;
    property AutoCompleteText: Boolean read FAutoCompleteText write FAutoCompleteText;
    property BoldNonDefaultValue: Boolean read FBoldNonDefaultValue write Set_BoldNonDefaultValue;
    property ClickTime: Integer read FClickTime write FClickTime;
    property ExtraRect[Index: Integer]: TRect read Get_ExtraRect;
    property GridColor: TColor read FGridColor write Set_GridColor;
    property GutterColor: TColor read FGutterColor write Set_GutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write Set_GutterEdgeColor;
    property GutterWidth: Integer read FGutterWidth write Set_GutterWidth;
    property HighlightColor: TColor read FHighlightColor write Set_HighlightColor;
    property ItemRect[Index: Integer]: TRect read Get_ItemRect;
    property NameColor: TColor read FNameColor write Set_NameColor;
    property NonDefaultValueColor: TColor read FNonDefaultValueColor write Set_NonDefaultValueColor;
    property PlusMinBtnRect[Index: Integer]: TRect read Get_PlusMinBtnRect;
    property PropTextRect[Index: Integer]: TRect read Get_PropTextRect;
    property ReadOnlyColor: TColor read FReadOnlyColor write Set_ReadOnlyColor;
    property ReferencesColor: TColor read FReferencesColor write Set_ReferencesColor;
    property SelectedIndex: Integer read FSelectedIndex;
    property SelectedItem: PwxPropertyItem read Get_SelectedItem;
    property ShowGridLines: Boolean read FShowGridLines write Set_ShowGridLines;
    property ShowGutter: Boolean read FShowGutter write Set_ShowGutter;
    property ShowItemHint: Boolean read FShowItemHint write FShowItemHint;
    property SubPropertiesColor: TColor read FSubPropertiesColor write Set_SubPropertiesColor;
    property TrackChange: Boolean read FTrackChange write FTrackChange;
    property ValueColor: TColor read FValueColor write Set_ValueColor;
    property ValueRect[Index: Integer]: TRect read Get_ValueRect;
    property ValueTextRect[Index: Integer]: TRect read Get_ValueTextRect;
    property OnCollapseItem: TzPropertyItemEvent read FOnCollapseItem write FOnCollapseItem;
    property OnExpandItem: TzPropertyItemEvent read FOnExpandItem write FOnExpandItem;
    property OnGetItemFriendlyName: TzPropertyItemGetFriendlyNameEvent read FOnGetItemFriendlyName write FOnGetItemFriendlyName;
    property OnGetItemReadOnly: TzPropertyItemEvent read FOnGetItemReadOnly write FOnGetItemReadOnly;
    property OnItemSetValue: TzPropertyItemSetValueEvent read FOnItemSetValue write FOnItemSetValue;
    property OnSelectItem: TzPropertyItemEvent read FOnSelectItem write FOnSelectItem;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TwxObjectInspector = class(TwxCustomObjectInspector)
  strict private
    class constructor Create;
    class destructor Destroy;
  published
    property Align;
    property AllowSearch;
    property AutoCompleteText;
    property BoldNonDefaultValue;
    property BorderStyle;
    property Color;
    property Component;
    property Ctl3d;
    property DefaultCategoryName;
    property DoubleBuffered;
    property FixedSplitter;
    property FloatPreference;
    property Font;
    property GridColor;
    property GutterColor;
    property GutterEdgeColor;
    property GutterWidth;
    property HeaderPropText;
    property HeaderValueText;
    property HighlightColor;
    property Hint;
    property NameColor;
    property NonDefaultValueColor;
    property ObjectVisibility;
    property OnAutoExpandItemOnInit;
    property OnBeforeAddItem;
    property OnClick;
    property OnCollapseItem;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpandItem;
    property OnGetItemFriendlyName;
    property OnGetItemReadOnly;
    property OnHeaderMouseDown;
    property OnItemSetValue;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnSplitterPosChanged;
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReferencesColor;
    property ShowGridLines;
    property ShowGutter;
    property ShowHeader;
    property ShowItemHint;
    property SortByCategory;
    property SplitterColor;
    property SplitterPos;
    property SubPropertiesColor;
    property TabOrder;
    property TabStop;
    property Text;
    property TrackChange;
    property ValueColor;
  end;

type
  /// <summary>
  ///   Base class for dialog-like editor.
  /// </summary>
  TwxObjectInspectorDialog = class abstract(TForm)
  private
    FPropertyItem: PwxPropertyItem;
    procedure SetPropertyItem(const AItem: PwxPropertyItem);
  protected
    procedure DoCreate; override;
    procedure Setup; virtual; abstract;
  public
    property PropertyItem: PwxPropertyItem read FPropertyItem write SetPropertyItem;
  end;

implementation

uses
  System.Math,
  System.RTLConsts,
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.Dialogs,
  Vcl.GraphUtil,
  Vcl.Styles,
  Vcl.Themes,
  wxObjectInspector.Exceptions,
  wxObjectInspector.Styles,
  wxObjectInspector.Utils,
  wxObjectInspector.ValueManager;


// https://blogs.msdn.microsoft.com/oldnewthing/20060428-00/?p=31373
// Why use CS_SAVEBITS - it is good for windows not move, covers a relatively
// small portion of the screen, and is visible for only a short time.
procedure TwxObjectInspectorListBox.CreateParams(var AParams: TCreateParams);
begin
  inherited;
  AParams.Style := AParams.Style or WS_BORDER;
  AParams.ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  AParams.WindowClass.Style := CS_SAVEBITS;
end;

procedure TwxObjectInspectorListBox.CreateWnd;
begin
  inherited CreateWnd;

  // Desktop as parent window so the list box is not constrained by the form. This way, if it
  // longer than the form, there will be no scrollbar.
  WinApi.Windows.SetParent(
    Handle, // hWndChild
    0       // hWndNewParent - NULL sets the desktop window as the new parent window.
  );

  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TwxObjectInspectorListBox.MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Self.HandleAllocated then
    ShowWindow(Self.Handle, SW_HIDE);
end;

procedure TwxObjectInspectorListBox.Set_Item(const AValue: Pointer);
begin
  FItem := AValue;
end;

procedure TwxObjectInspectorListBox.Set_ItemEdit(const AValue: TCustomEdit);
begin
  FItemEdit := AValue;
end;

procedure TwxObjectInspectorCustomListBox.CreateWnd;
begin
  inherited;
  PopulateList;
end;

// The list box is owner-drawn, but each item in the list box is the height specified by
// the ItemHeight property. Each time an item is displayed in an lbOwnerDrawFixed list box,
// the OnDrawItem event occurs. The event handler for OnDrawItem draws the specified item.
// The ItemHeight property determines the height of each of the items.
constructor TwxColorListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawFixed;
end;

procedure TwxColorListBox.ColorCallBack(const AName: string);
var
  color: TColor;
begin
  color := StringToColor(AName);
  Items.AddObject(AName, TObject(color));
end;

procedure TwxColorListBox.DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState);
const
  cColorCode = $C0;
  cTextMargin = 5;
  cWidthAdjustment = 5;
function colorToBorderColor(AColor: TColor; AOwnerDrawState: TOwnerDrawState): TColor;
begin
  if (Byte(AColor) > cColorCode) or                    { Red }
     (Byte(AColor shr SizeOf(Byte)) > cColorCode) or   { Green }
     (Byte(AColor shr SizeOf(Word)) > cColorCode) then { Blue }
    Result := clBlack
  else
    if odSelected in AOwnerDrawState then
      Result := clWhite
    else
      Result := AColor;
end;
var
  colorSampleRect: TRect;
  textColor: TColor;
begin
  Canvas.FillRect(ARect); // To paint the original rect
  textColor := Canvas.Brush.color;

  colorSampleRect := ARect;
  // Reduce the rect, make it a little bit rectangle, with width = height + widthAdjustment
  colorSampleRect.Right := colorSampleRect.Bottom - colorSampleRect.Top + colorSampleRect.Left + cWidthAdjustment;
  // Shrink for 1 pixel.
  InflateRect(colorSampleRect, - 1, - 1);
  // Paint the rect using the indexed color
  Canvas.Brush.color := Colors[AIndex];
  Canvas.FillRect(colorSampleRect);
  // Make a frame
  Canvas.Brush.color := colorToBorderColor(ColorToRGB(Brush.color), AOwnerDrawState);
  Canvas.FrameRect(colorSampleRect);
  // Assign color for drawing the text
  Canvas.Brush.color := textColor;
  ARect.Left := colorSampleRect.Right + cTextMargin; // Shift Rect

  // Put text in the middle of the rect.
  Canvas.TextRect(
    ARect,
    ARect.Left,
    ARect.Top + (ARect.Height - Canvas.TextHeight(Items[AIndex])) div 2,
    Self.Items[AIndex]
  );
end;

function TwxColorListBox.GetColor(AIndex: Integer): TColor;
begin
  Result := TColor(Items.Objects[AIndex]);
end;

procedure TwxColorListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  GetColorValues(ColorCallBack);
  Items.EndUpdate;
end;

constructor TwxCursorListBox.Create(AOwner: TComponent);
const
  cDefaultItemHeight = 35;
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawFixed;
  ItemHeight := cDefaultItemHeight;
end;

procedure TwxCursorListBox.CursorCallBack(const AName: string);
var
  cursor: TCursor;
begin
  cursor := StringToCursor(AName);
  Items.AddObject(AName, TObject(Cursor));
end;

procedure TwxCursorListBox.DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState);
const
  cCursorHeight = 32;
  cCursorWidth = 32;
  cIcoRectBorderMargin = 2;
  cIcoRectMargin = 5;
  cIcoRectWidthAdjustment = 5;
var
  cursor: TCursor;
  icoRect: TRect;
begin
  cursor := Self.Cursors[AIndex];
  icoRect := ARect; // Assign the original rect
  icoRect.Right := icoRect.Left + icoRect.Height + cIcoRectWidthAdjustment;
  Canvas.FillRect(ARect);
  InflateRect(icoRect, -cIcoRectBorderMargin, -cIcoRectBorderMargin);

  // Draw icon inside icoRect
  DrawIconEx(
    Canvas.Handle,
    cIcoRectMargin + icoRect.Left, // Add the margin to the left side
    icoRect.Top,
    Screen.Cursors[Cursor],
    cCursorWidth,
    cCursorHeight,
    0,
    0,
    DI_NORMAL
  );

  // Add the margin to the right side
  ARect.Left := icoRect.Right + cIcoRectMargin;

  // Draw text in the middle of ARect.
  Canvas.TextRect(
    ARect,
    ARect.Left,
    ARect.Top + (ARect.Height - Canvas.TextHeight(Items[AIndex])) div 2,
    Items[AIndex]
  );
end;

function TwxCursorListBox.Get_Cursor(AIndex: Integer): TCursor;
begin
  Result := TCursor(Items.Objects[AIndex]);
end;

procedure TwxCursorListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  GetCursorValues(CursorCallBack);
  Items.EndUpdate;
end;

constructor TwxShortcutListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TwxShortcutListBox.EnumShortCuts;
var
  vkCode: Byte;
  I: Integer;
const
  MultiKeysArray: array [0 .. 5] of TShortCut = (scCtrl, scCtrl + scAlt, scCommand,
    scCtrl + scCommand, scShift + scCommand, scCommand + scAlt);
  UniqueKeysArray: array [0 .. 3] of TShortCut = (scShift, scCtrl, scAlt, scCommand);

  procedure AddShortCut(ShortCut: TShortCut);
  begin
    Items.AddObject(ShortCutToTextEx(ShortCut), TObject(ShortCut));
  end;
begin
  AddShortCut(scNone);

  { Keys + (A .. Z) }
  for I := Low(MultiKeysArray) to High(MultiKeysArray) do begin
    for vkCode := vkA to vkZ do AddShortCut(vkCode or MultiKeysArray[I]);
  end;

  { Keys + (F1 .. F12) }
  for I := Low(MultiKeysArray) to High(MultiKeysArray) do begin
    for vkCode := vkF1 to vkF12 do AddShortCut(vkCode or MultiKeysArray[I]);
  end;

  { Key + (Insert,Delete,Enter,Esc) }
  for I := Low(UniqueKeysArray) to High(UniqueKeysArray) do begin
    AddShortCut(UniqueKeysArray[I] + vkInsert); // Insert
    AddShortCut(UniqueKeysArray[I] + vkDelete); // Delete
    AddShortCut(UniqueKeysArray[I] + vkReturn); // Enter
    AddShortCut(UniqueKeysArray[I] + vkEscape); // Esc
  end;

  { Special ShortCuts }
  AddShortCut(scAlt + vkBack); // Alt+BkSp
  AddShortCut(scShift + scAlt + vkBack); // Shift+Alt+BkSp
end;

function TwxShortcutListBox.Get_ShortCut(AIndex: Integer): TShortCut;
begin
  Result := TShortCut(Items.Objects[AIndex]);
end;

procedure TwxShortcutListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  EnumShortCuts;
  Items.EndUpdate;
end;

class constructor TwxObjectInspectorButton.Create;
begin
  FScaledWidth := DefaultWidth;
  FScaledArrowSize := DefaultArrowSize;
end;

constructor TwxObjectInspectorButton.Create(AOwner: TComponent);
begin
  inherited;
  FDropDown := True;
end;

procedure TwxObjectInspectorButton.Paint;
var
  arrowLocation: TPoint;
  cliRect: TRect;
  oldFontStyles: TFontStyles;
  oldPenColor: TColor;
  themeDetails: TThemedElementDetails;
begin
  cliRect := Self.ClientRect;

  if IsMouseDown and IsMouseInControl then
    themeDetails := StyleServices.GetElementDetails(tbPushButtonPressed)
  else
    themeDetails := StyleServices.GetElementDetails(tbPushButtonNormal);

  // Has transparent parts, so draw parent backgroud first
  if StyleServices.HasTransparentParts(themeDetails) then
    StyleServices.DrawParentBackground(Handle, Canvas.Handle, nil, False);

  // Then draw element of self
  StyleServices.DrawElement(Canvas.Handle, themeDetails, ClientRect);

  if FDropDown then begin
    oldPenColor := Canvas.Pen.Color;
    Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText);
    // Arrow height = size; arrow length = 2 x size.
    arrowLocation := Point(Width div 2 - ScaledArrowSize, Height div 2 - ScaledArrowSize div 2);
    DrawArrow(Canvas, sdDown, arrowLocation, ScaledArrowSize);
    Canvas.Pen.Color := oldPenColor;
  end else begin
    oldFontStyles := Canvas.Font.Style;

    if StyleServices.IsSystemStyle then
      Canvas.Font.Style := [fsBold];

    StyleServices.DrawText(
      Canvas.Handle,
      themeDetails,
      '...',
      cliRect,
      [tfCenter, tfVerticalCenter, tfSingleLine]
    );

    Canvas.Font.Style := oldFontStyles;
  end;
end;

class procedure TwxObjectInspectorButton.Set_ScaledArrowSize(const AValue: Integer);
begin
  FScaledArrowSize := AValue;
end;

class procedure TwxObjectInspectorButton.Set_ScaledWidth(const AValue: Integer);
begin
  FScaledWidth := AValue;
end;

procedure TwxObjectInspectorButton.WMLButtonDown(var AMessage: TWMLButtonDown);
begin
  inherited;
  Invalidate; // So as to Paint
end;

procedure TwxObjectInspectorButton.WMLButtonUp(var AMessage: TWMLButtonUp);
begin
  inherited;
  Invalidate; // So as to Paint
end;

constructor TwxObjectInspectorEdit.Create(AOwnerInspector: TwxCustomControl);
begin
  inherited Create(AOwnerInspector);
  ParentCtl3D := False;
  BorderStyle := TBorderStyle(0);
  Ctl3D := False;
  TabStop := False;
  FListBox := nil;
  FPropertyItem := nil;
  FOwnerInspector := AOwnerInspector;
  FButton := TwxObjectInspectorButton.Create(Self);
  FButton.OnMouseDown := ButtonClick;
end;

procedure TwxObjectInspectorEdit.ButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // When Button is visible, HasButton must be True, meaning either ListBox, or Dialog
  if Assigned(FListBox) then begin
    if IsWindowVisible(FListBox.Handle) then
      HideList
    else
      ShowList;
  end else begin
    ShowModalDialog;
  end;
end;

procedure TwxObjectInspectorEdit.CMCancelMode(var AMessage: TCMCancelMode);
begin
  inherited;
  // CMCancelMode is generated whenever mouse left button is down. We would like
  // to only respond to the Sender Edit.
  if (AMessage.Sender = FButton) or (AMessage.Sender = FListBox) then Exit;

  if Assigned(FListBox) then begin
    if IsWindowVisible(FListBox.Handle) then begin
      HideList;
      DoSetValueFromList;
    end;
  end;
end;

procedure TwxObjectInspectorEdit.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  FButton.Visible := Self.Visible;
  UpdateButton;
end;

procedure TwxObjectInspectorEdit.DoDblClick;
var
  listIndex: Integer;
begin
  inherited;

  if ValueManager.HasDialog(FPropertyItem) then begin
    ShowModalDialog
  end else begin
    if Assigned(FListBox) then begin
      if FListBox.Items.Count > 0 then begin
        listIndex := FListBox.ItemIndex + 1;

        if listIndex >= FListBox.Items.Count then
          listIndex := 0;

        FListBox.Selected[listIndex] := True;
        DoSetValueFromList;
      end;
    end;
  end;
end;

procedure TwxObjectInspectorEdit.DoSetValueFromEdit;
var
  str: string;
  uint64Val: UInt64;
  value: TValue;
  listIndex: Integer;
begin
 if not Assigned(FPropertyItem) then
   Exit;

  str := Text;
  // If the Edit is emptied, then we will always do a value update!
  if (not FTextChanged) and (not str.Trim.IsEmpty) then
    Exit;

  FTextChanged := False;

  if (ReadOnly) or (FPropertyItem^.IsSet) or (not FPropertyItem^.AssociatedProperty.IsWritable) then
    Exit;

  if str.IsEmpty then begin
    uint64Val := 0;
    value := ValueManager.GetValue(FPropertyItem, uint64Val);
    // Via ispector so proper events can be fired by inspector. Otherwise we can direclty set to
    // the property itme.
    TwxCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, value);
    Exit;
  end else begin
    if Assigned(FListBox) then begin
      listIndex := FListBox.Items.IndexOf(str);

      if listIndex > - 1 then begin
        FListBox.Selected[listIndex] := True;
        DoSetValueFromList;
        Exit;
      end else begin
        if not ValueManager.ValueHasOpenProbabilities(FPropertyItem) then begin
          raise EInvalidPropertyValue.Create(FPropertyItem.Name, str);
          Exit; // Is this exit necessary?
        end;
      end
    end;
  end;

  value := ValueManager.StrToValue(FPropertyItem, str);
  TwxCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, value);
  SelectAll;
end;

procedure TwxObjectInspectorEdit.DoSetValueFromList;
var
  method: TMethod;
  newValue: TValue;
  obj: TObject;
begin
  newValue := TValue.Empty;

  if ReadOnly or (FListBox.ItemIndex < 0) then
    Exit;

  // Value has been set via DoSetValueFromList before, so Exit because no need to do it again.
  if FDefaultSelectedIndex = FListBox.ItemIndex then
    Exit;

  obj := (FListBox.Items.Objects[FListBox.ItemIndex]);

  if FPropertyItem.AssociatedProperty.PropertyType.TypeKind = tkMethod then begin
    method.Code := obj;
    method.Data := FPropertyItem.AssociatedComponentParentForm;
    newValue := ValueManager.GetValue(FPropertyItem, method);
  end else begin
    newValue := ValueManager.GetValue(FPropertyItem, obj);
  end;

  TwxCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, newValue);
  // Update the log of the selected index.
  FDefaultSelectedIndex := FListBox.ItemIndex;
end;

function TwxObjectInspectorEdit.Get_ListBox: TwxObjectInspectorListBox;
begin
  Result := FListBox;
end;

function TwxObjectInspectorEdit.Get_PropertyItem: PwxPropertyItem;
begin
  Result := FPropertyItem;
end;

{**
Query HandleAllocated to find out if the control's underlying screen object has been generated.
If the screen object exists, HandleAllocated returns true. If the screen object does not exist,
HandleAllocated returns false. Testing the Handle property of a control directly causes the window
to be created if it does not already exist. Call the HandleAllocated method to determine
whether a window exists without creating one as a side effect.
**}
procedure TwxObjectInspectorEdit.HideList;
begin
  if Assigned(FListBox) and (FListBox.HandleAllocated) then
    ShowWindow(FListBox.Handle, SW_HIDE);
end;

procedure TwxObjectInspectorEdit.InitList;
var
  selIndex: Integer;
  str: string;
begin
  // Release the existing ListBox control, because the currently type may need a diff list box.
  if Assigned(FListBox) then
    FreeAndNil(FListBox);

  // We cannot use ReadOnly here, but must use IsWritable to determine ListBox visibility
  // because Set/Class will make the Edit ReadOnly, but we still need the list box!
  if not FPropertyItem.AssociatedProperty.IsWritable then
    Exit;

  if not ValueManager.HasListBox(FPropertyItem) then begin
    if (ValueManager.HasDialog(FPropertyItem)) then
      FButton.DropDown := False;
    // As long as HasNoListBox, Exit.
    Exit;
  end;

  FListBox := TzObjectInspectorListBoxClass(ValueManager.GetListBoxClass(PropertyItem)).Create(Self);
  FListBox.Item := FPropertyItem;
  FListBox.ItemEdit := Self;
  FListBox.Visible := False; // Initialize to False.
  FListBox.Parent := Self.Parent; // Parent is the same as the Edit's parent, i.e., the inpsector
  FListBox.OnMouseDown := ListBoxMouseDown;

  // Get list items.
  if not (FListBox is TwxObjectInspectorCustomListBox) then
    ValueManager.GetListItems(FPropertyItem, FListBox.Items);

  // Select the default item value
  str := FPropertyItem.ValueAsString;
  selIndex := FListBox.Items.IndexOf(str);

  //Set selected.
  if selIndex > - 1 then
    FListBox.Selected[selIndex] := True;

  FDefaultSelectedIndex := selIndex;
  FButton.DropDown := True;
end;

procedure TwxObjectInspectorEdit.ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoSetValueFromList;
end;

procedure TwxObjectInspectorEdit.PropertyItemChanged;
var
  inspector: TwxCustomObjectInspector;
begin
  // If the newly changed value is nil (which is possible), then exit.
  if not Assigned(FPropertyItem) then
    Exit;

  // Reset TextChanged flag to False.
  FTextChanged := False;
  // Type cast from TwxCustomControl to TwxCustomObjectInspector.
  inspector := TwxCustomObjectInspector(FOwnerInspector);

  // Determine ReadOnly
  if inspector.ReadOnly then begin
    Self.ReadOnly := True;
  end else begin
    Self.ReadOnly := not FPropertyItem^.AssociatedProperty.IsWritable;
    if (FPropertyItem^.IsSet) or (FPropertyItem^.IsClass) then
      Self.ReadOnly := True;
  end;

  // Invoke OnGetItemReadOnly Event, when ReadOnly = False, giving the user an opportunity to
  // override.
  if not Self.ReadOnly then begin
    if Assigned(inspector.OnGetItemReadOnly) then
      Self.ReadOnly := inspector.OnGetItemReadOnly(FOwnerInspector, FPropertyItem);
  end;

  // Determine font color, depending if the edit is read-only or not.
  if ReadOnly and (not (FPropertyItem^.IsSet or FPropertyItem^.IsClass)) then
    Font.Color := inspector.ReadOnlyColor
  else
    Font.Color := GetSysColor(clWindowText);

  // Initialize the internal list box control.
  InitList;
end;

procedure TwxObjectInspectorEdit.Set_PropertyItem(const AItem: PwxPropertyItem);
begin
  if not FPropertyItem.EqualTo(AItem) then begin
    FPropertyItem := AItem;
    PropertyItemChanged;
  end;
end;

procedure TwxObjectInspectorEdit.SetValueFromEdit;
begin
  DoSetValueFromEdit;
end;

procedure TwxObjectInspectorEdit.ShowList;
const
  cListBoxBottomPositionMargin = 50;
  cListBoxHeightMargin = 4;
  cListBoxTopPositionMargin = 2;
var
  inspector: TwxCustomObjectInspector;
  listBoxPos: TPoint;
  newListBoxHeight: Integer;
begin
  if (not Assigned(FListBox)) or (FListBox.Items.Count = 0) then
    Exit;

  inspector := TwxCustomObjectInspector(FOwnerInspector);

  // Determining the Height and Width of the ListBox
  // Get the Height of the entire list box
  newListBoxHeight := FListBox.ItemHeight * (FListBox.Items.Count) + cListBoxHeightMargin;
  // List Box Pos.X = SplitterPos, List Pos.Y = Edit Bottom + cListTopPositionMargin
  listBoxPos := Point(inspector.SplitterPos, Top + Height + cListBoxTopPositionMargin);
  // Convert to Screen Pos
  listBoxPos := Parent.ClientToScreen(listBoxPos);

  // Adjust if the ListBox goes beyond the bottom of the Screen. The goal is the have the bottom
  // of the adjusted list box to have a distance of 50 to the bottom of the screen. Note: in the
  // following light, newListBoxHeight could be negative.
  if (newListBoxHeight + listBoxPos.Y) >= Screen.Height then
    newListBoxHeight := Screen.Height - listBoxPos.Y - cListBoxBottomPositionMargin;

  FListBox.Height := newListBoxHeight;//If value negative,the FListBox.Height internally will set 0.
  FListBox.Width  := inspector.ClientWidth - inspector.SplitterPos;

  SetWindowPos(
    FListBox.Handle, // hWnd
    HWND_TOP,        // hWndInsertAfter. HWND_TOP: Places the window at the top of the Z order.
    listBoxPos.X,    // X: The new position of the left side of the window, in client coordinates.
    listBoxPos.Y,    // Y: The new position of the top of the window, in client coordinates.
    0,               // cx:The new width of the window, in pixels.
    0,               // cy:The new height of the window, in pixels.
    SWP_NOSIZE or    // SWP_NO_SIZE: Retains current size ignoring cx/cy).
    SWP_NOACTIVATE or// SWP_NOACTIVATE: Does not activate the window.
    SWP_SHOWWINDOW   // SWP_SHOWWINDOW: Displays the window
  );
end;

procedure TwxObjectInspectorEdit.ShowModalDialog;
var
  dlg: TComponent;
  dlgClass: TComponentClass;
  dlgResult: TValue;
  mr: Integer;
begin
  dlgClass := nil;

  if not FPropertyItem^.AssociatedProperty.IsWritable then
    Exit;

  if ValueManager.HasDialog(FPropertyItem) then
    dlgClass := ValueManager.GetDialog(FPropertyItem);

  if Assigned(dlgClass) then begin
    dlg := dlgClass.Create(Self);

    if (not (dlg is TCommonDialog)) and (not (dlg is TwxObjectInspectorDialog)) then
      raise EInvalidDialogClass.Create;

    ValueManager.PerformDialogAction(FPropertyItem, dlg, dcInit);
    mr := ValueManager.PerformDialogAction(FPropertyItem, dlg, dcShow);
    PostMessage(Handle, WM_LBUTTONUP, 0, 0);

    if mr = mrOk then begin
      ValueManager.PerformDialogAction(FPropertyItem, dlg, dcFinished);
      dlgResult := ValueManager.DialogResultValue(FPropertyItem, dlg);
      TwxCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, dlgResult);
    end;

    ValueManager.PerformDialogAction(FPropertyItem, dlg, dcBeforeDestroy);
    FreeAndNil(dlg);
  end;
end;

procedure TwxObjectInspectorEdit.UpdateButton;
const
  cButtonTopPositionMargin = 3;
var
  inspector: TwxCustomObjectInspector;
begin
  FButton.Visible := False; // Reset to invisible.
  inspector := TwxCustomObjectInspector(FOwnerInspector);

  if (not Assigned(FPropertyItem)) then
    Exit;

  if (inspector.SelectedIndex < 0) or (inspector.SelectedIndex > inspector.VisiblePropCount) then
    Exit;

  if not FPropertyItem^.AssociatedProperty.IsWritable then
    Exit;

  // HasButton must be True!
  if not ValueManager.HasButton(PropertyItem) then
    Exit;

  FButton.Parent := Self.Parent; // Parent is inspector.
  FButton.Left := Self.Parent.ClientWidth - TwxObjectInspectorButton.ScaledWidth;
  FButton.Top := Top - cButtonTopPositionMargin;
  FButton.Height := inspector.ItemHeight;
  FButton.Width := TwxObjectInspectorButton.ScaledWidth;
  FButton.Visible := True;
end;

procedure TwxObjectInspectorEdit.UpdateEditText;
var
  inspector: TwxCustomObjectInspector;
begin
  inspector := TwxCustomObjectInspector(FOwnerInspector);
  Self.Text := FPropertyItem.ValueAsString;

  if not inspector.DefaultPropertyValue.ContainsKey(FPropertyItem.QualifiedName) then
    inspector.DefaultPropertyValue.Add(FPropertyItem.QualifiedName, Self.Text);

  SelectAll;
end;

procedure TwxObjectInspectorEdit.WMChar(var AMessage: TWMChar);
var
  findText: string;
  I: Integer;
begin
  case AMessage.CharCode of
    vkReturn:
      begin
        FTextChanged := True;
        DoSetValueFromEdit;
        AMessage.Result := 0; // No beep !
        Exit;
      end;
    vkClear:
      begin
        FTextChanged := True;
        inherited;
        Exit;
      end;
    vkBack:
      begin
        inherited;
        Exit;
      end;
  else
    begin
      inherited;
      FTextChanged := True;
    end;
  end;

  if not TwxCustomObjectInspector(FOwnerInspector).AutoCompleteText then
    Exit;

  { Auto complete user input text }
  findText := Text;

  if Assigned(FListBox) then begin
    for I := 0 to FListBox.Items.Count - 1 do begin
      if StartsWith(findText, FListBox.Items[I]) then begin
        Text := FListBox.Items[I];
        SelStart := Length(findText);
        SelLength := Length(FListBox.Items[I]) - 1;
        Exit;
      end;
    end;
  end;
end;

procedure TwxObjectInspectorEdit.WMKeyDown(var AMessage: TWMKeyDown);
var
  isToList: Boolean;
  selectedIndex: Integer;
begin
  isToList := Assigned(FListBox) and IsWindowVisible(FListBox.Handle);

  { isToList = True  ==> Keyboard message will translated to the ListBox .
    isToList = False ==> Keyboard message will be used to select item from the Inspector. }
  if isToList then
    selectedIndex := FListBox.ItemIndex
  else
    selectedIndex := TwxCustomObjectInspector(FOwnerInspector).SelectedIndex;

  case AMessage.CharCode of
    vkReturn:
      begin
        if isToList then begin
          HideList;
          DoSetValueFromList;
          Exit;
        end;
      end;
    vkUp:
      begin
        Dec(selectedIndex);
        selectedIndex := max(0, selectedIndex);
      end;
    vkDown:
      begin
        Inc(selectedIndex);
        selectedIndex := Min(TwxCustomObjectInspector(FOwnerInspector).VisiblePropCount - 1, selectedIndex);
      end;
  else
    begin
      inherited;
      Exit;
    end;
  end;

  // Must inherite before calling SelectItem ! ==> In order that SelectAll works properly .
  inherited;
  if isToList then
    FListBox.Selected[selectedIndex] := True
  else
    TwxCustomObjectInspector(FOwnerInspector).SelectItem(selectedIndex);
end;

procedure TwxObjectInspectorEdit.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;

  if Assigned(FListBox) then begin
    if IsWindowVisible(FListBox.Handle) then
      HideList;
  end;

  DoSetValueFromEdit;
  TwxCustomObjectInspector(FOwnerInspector).UnRegisterKeys;
end;

procedure TwxObjectInspectorEdit.WMLButtonDblClk(var AMessage: TWMLButtonDblClk);
begin
  DoDblClick;
end;

// Useless function for WMLButtonDown?
procedure TwxObjectInspectorEdit.WMLButtonDown(var AMessage: TWMLButtonDown);
const
  cMinDoubleClickGap = 200;
var
  curClickTime, elapsedTime: Integer;
begin
  { When PropInspEdit is activated, the Inspector will not fire the WMLBUTTONDBLCLK message .
    => we need to detect the double click manually! }
  if TwxCustomObjectInspector(FOwnerInspector).ClickTime <> - 1 then begin
    curClickTime := GetTickCount;
    elapsedTime := curClickTime - TwxCustomObjectInspector(FOwnerInspector).ClickTime;

    if (elapsedTime) < cMinDoubleClickGap then
      DoDblClick;

    TwxCustomObjectInspector(FOwnerInspector).ClickTime := - 1;
  end;
  inherited;
end;

// Every focus we need to register key.  This means, every time we click the Edit's window
// a HotKey registration will occur. Then, we press the registered hot key (TAB), the caret will
// be moved to the property name column, causing the Edit losing focus hence unregister the hot
// key. After the caret is moved to  the property name column, the user can press keys to do the
// live serach.
// Check TwxObjectInspectorEdit.WMKillFocus, and TwxCustomObjectInspector.WMHotKey
procedure TwxObjectInspectorEdit.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;
  TwxCustomObjectInspector(FOwnerInspector).RegisterKeys;
end;

procedure TwxObjectInspectorEdit.WMWindowPosChanged(var AMessage: TWMWindowPosChanged);
begin
  inherited;
  UpdateButton;
end;

procedure TwxObjectInspectorEdit.WndProc(var AMessage: TMessage);
begin
  { * WM_ACTIVATE is sent with WA_ACTIVE when a window is activated by some method other
    than a mouse click (for example, by a call to the SetActiveWindow function or by use
    of the keyboard interface to select the window). So we intercept WM_ACTIVATE here. }
  case AMessage.Msg of
    WM_ACTIVATE:
      begin
        AMessage.WParam := WA_INACTIVE;
        AMessage.Result := 0;
        Exit;
      end;
  end;
  inherited WndProc(AMessage);
end;

constructor TwxFloatPreference.Create;
const
  defExpPrecision = 2;
  defMaxDigits = 6;
begin
  inherited;
  FExpPrecision := defExpPrecision;
  FMaxDigits := defMaxDigits;
end;

procedure TwxFloatPreference.Assign(ASource: TPersistent);
begin
  if ASource is TwxFloatPreference then begin
    MaxDigits := TwxFloatPreference(ASource).MaxDigits;
    ExpPrecision := TwxFloatPreference(ASource).ExpPrecision;
  end else begin
    inherited Assign(ASource);
  end;
end;

procedure TwxPropertyItem.CheckOwnerList;
begin
  if not Assigned(FOwnerList) then raise EItemOwnerListNotAssigned.Create;
end;

class function TwxPropertyItem.Empty: TwxPropertyItem;
begin
  ZeroMemory(@Result, SizeOf(TwxPropertyItem));
end;

function TwxPropertyItem.EqualTo(AItemToCompare: PwxPropertyItem): Boolean;
begin
  Result := @Self = AItemToCompare;
end;

function TwxPropertyItem.Get_AssociatedComponentParentForm: TCustomForm;
begin
  Result := GetComponentParentForm(Component);
end;

function TwxPropertyItem.Get_AssociatedProperty: TRttiProperty;
begin
  Result := FAssociatedProperty;
end;

procedure TwxPropertyItem.Set_AssociatedProperty(const AValue: TRttiProperty);
begin
  FAssociatedProperty := AValue;
end;

function TwxPropertyItem.Get_CategoryIndex: Integer;
begin
  Result := FCategoryIndex;
end;

procedure TwxPropertyItem.Set_CategoryIndex(const AValue: Integer);
begin
  FCategoryIndex := AValue;
end;

function TwxPropertyItem.Get_CategoryName: string;
begin
  Result := FCategoryName;
end;

procedure TwxPropertyItem.Set_CategoryName(const AValue: string);
begin
  FCategoryName := AValue;
end;

function TwxPropertyItem.Get_ChildCount: Integer;
var
  selfPos, I: Integer;
begin
  CheckOwnerList;
  Result := 0;

  if not MayHaveChild then
    Exit;

  selfPos := FOwnerList.IndexOf(@Self);

  if selfPos > - 1 then begin
    for I := selfPos + 1 to FOwnerList.Count - 1 do begin
      // Search till next category index is changed to a different
      // If any item has IsCategory = True, that means the inspector is being showned in terms of
      // category.
      if IsCategory then begin
        if FOwnerList.Items[I].CategoryIndex = CategoryIndex then
          Inc(Result)
        else
          if FOwnerList.Items[I].CategoryIndex <> -1 then Break;
      end else begin
        // Check parents to get child!
        if FOwnerList.Items[I].Parent = @Self then
          Inc(Result)
        else
          if FOwnerList.Items[I].Parent = Parent then Break;
      end;
    end;
  end;
end;

function TwxPropertyItem.Get_ChildItems(const AIndex: Integer): PwxPropertyItem;
var
  childList: TList;
  selfPos, I: Integer;
begin
  CheckOwnerList;
  Result := nil;

  if (AIndex < 0) or (not MayHaveChild) then
    Exit;

  childList := TList.Create;
  try
    selfPos := FOwnerList.IndexOf(@Self);

    if selfPos > - 1 then begin
      for I := selfPos + 1 to FOwnerList.Count - 1 do begin
        if IsCategory then begin
          if FOwnerList.Items[I].CategoryIndex = CategoryIndex then
            childList.Add(FOwnerList.Items[I]);
        end else begin
          if FOwnerList.Items[I].Parent = @Self then
            childList.Add(FOwnerList.Items[I]);
        end;

        if childList.Count > AIndex then Break;
      end;

      if AIndex < childList.Count then
        Result := childList.Items[AIndex];
    end;
  finally
    childList.Free;
  end;
end;

function TwxPropertyItem.Get_Component: TObject;
begin
  Result := FComponent;
end;

procedure TwxPropertyItem.Set_Component(const AValue: TObject);
begin
  FComponent := AValue;
end;

function TwxPropertyItem.Get_Expanded: Boolean;
begin
  CheckOwnerList;
  Result := ChildCount > 0;
  if Result then Result := Assigned(ChildItems[0]);
  if Result then Result := ChildItems[0].Visible;
end;

procedure TwxPropertyItem.Set_FloatPreference(const AValue: TwxFloatPreference);
begin
  FFloatPreference := AValue;
end;

function TwxPropertyItem.Get_HasChild: Boolean;
begin
  if IsCategory then
    Exit(True);

  if (not MayHaveChild) then
    Exit(False);   // Category, Set, and Class.

  Result := not Value.IsEmpty;

  if Result and IsClass then
    Result := ObjectHasAtLeastOneChild(Value.AsObject, ObjectVisibility);
end;

function TwxPropertyItem.Get_Instance: TObject;
begin
  Result := FInstance;
end;

procedure TwxPropertyItem.Set_Instance(const AValue: TObject);
begin
  FInstance := AValue;
end;

function TwxPropertyItem.Get_IsCategory: Boolean;
begin
  Result := FIsCategory;
end;

procedure TwxPropertyItem.Set_IsCategory(const AValue: Boolean);
begin
  FIsCategory := AValue;
end;

function TwxPropertyItem.Get_IsClass: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := AssociatedProperty.PropertyType.TypeKind = tkClass;
end;

function TwxPropertyItem.Get_IsComponent: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkClass) and
    IsClassDerivedFromClass(TRttiInstanceType(AssociatedProperty.PropertyType).MetaclassType, TComponent);
end;

function TwxPropertyItem.Get_IsEmpty: Boolean;
var
  emptyItem: TwxPropertyItem;
begin
  emptyItem := TwxPropertyItem.Empty;
  Result := CompareMem(@Self, @emptyItem, SizeOf(TwxPropertyItem));
end;

function TwxPropertyItem.Get_IsEnum: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := AssociatedProperty.PropertyType.TypeKind = tkEnumeration;
end;

function TwxPropertyItem.Get_IsSet: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkSet) and (SetElementValue = - 1);
end;

function TwxPropertyItem.Get_IsSetElement: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkSet) and (SetElementValue > - 1);
end;

function TwxPropertyItem.Get_MayHaveChild: Boolean;
begin
  Result := IsCategory or IsClass or IsSet;
end;

function TwxPropertyItem.Get_Name: string;
begin
  if IsCategory then begin
    Result := CategoryName;
    Exit;
  end;

  if Get_IsSetElement then
    Result := GetEnumName(AssociatedProperty.PropertyType.AsSet.ElementType.Handle, SetElementValue)
  else
    Result := AssociatedProperty.Name;
end;

function TwxPropertyItem.Get_ObjectVisibility: TMemberVisibility;
begin
  Result := FObjectVisibility;
end;

procedure TwxPropertyItem.Set_ObjectVisibility(const AValue: TMemberVisibility);
begin
  FObjectVisibility := AValue;
end;

function TwxPropertyItem.Get_Parent: PwxPropertyItem;
begin
  Result := FParent;
end;

procedure TwxPropertyItem.Set_Parent(const AValue: PwxPropertyItem);
begin
  FParent := AValue;
end;

function TwxPropertyItem.Get_QualifiedName: string;
begin
  Result := FQualifiedName;
end;

procedure TwxPropertyItem.Set_QualifiedName(const AValue: string);
begin
  FQualifiedName := AValue;
end;

function TwxPropertyItem.Get_SetElementValue: Integer;
begin
  Result := FSetElementValue;
end;

procedure TwxPropertyItem.Set_SetElementValue(const AValue: Integer);
begin
  FSetElementValue := AValue;
end;

function TwxPropertyItem.Get_Value: TValue;
begin
  Result := TValue.Empty;

  if IsCategory then
    Exit;

  if Assigned(Instance) then
    Result := AssociatedProperty.GetValue(Instance);
end;

function TwxPropertyItem.Get_ValueAsString: string;
var
  obj: TObject;
begin
  Result := Self.Value.ToString;
  // Current item is set element, its value name will be "True" or "False", depending on
  // whether the set element is included in the set already.
  if Self.IsSetElement then begin
    Result := BoolToI18nStr(ElementInSet(GetSetOrdinalValue(Self.Value), Self.SetElementValue));
    Exit;
  end;

  // Color -> color name string
  if (Self.Value.TypeInfo = TypeInfo(TColor)) then begin
    Result := ColorToString(TValueConverter.GetValueAs<TColor>(Self.Value));
    Exit;
  end;

  // Cursor -> cursor name string
  if (Self.Value.TypeInfo = TypeInfo(TCursor)) then begin
    Result := CursorToString(TValueConverter.GetValueAs<TCursor>(Self.Value));
    Exit;
  end;

  // Shortcut -> shortcut string
  if (Self.Value.TypeInfo = TypeInfo(TShortCut)) then begin
    Result := ShortCutToTextEx(TValueConverter.GetValueAs<TShortCut>(Self.Value));
    Exit;
  end;

  // Object -> Object Name, or Component Name string
  if Self.Value.IsObject then begin
    Result := EmptyStr;

    if not Self.Value.IsEmpty then begin
      obj := TValueConverter.GetValueAs<TObject>(Self.Value);
      Result := Format('(%s)', [obj.ToString]);

      if ((obj is TComponent) and (TComponent(obj).Name <> '')) then
        Result := TComponent(obj).Name;
    end;

    Exit;
  end;

  // Method (event handler) will be the method name, as get from its parent form.
  if Self.AssociatedProperty.PropertyType.TypeKind = tkMethod then begin
    Result := GetMethodName(Self.Value, Self.AssociatedComponentParentForm);
    Exit;
  end;

  // Single -> string format single
  if Self.Value.TypeInfo = TypeInfo(Single) then begin
    Result := TValueData(Self.Value).FAsSingle.ToString(ffGeneral, FFloatPreference.ExpPrecision, FFloatPreference.MaxDigits);
    Exit;
  end;

  // Double -> string format double
  if Self.Value.TypeInfo = TypeInfo(Double) then begin
    Result := Self.Value.AsExtended.ToString(ffGeneral, FFloatPreference.ExpPrecision, FFloatPreference.MaxDigits);
    Exit;
  end;

  // Extended -> string format extended
  if Self.Value.TypeInfo = TypeInfo(Extended) then begin
    Result := Self.Value.AsExtended.ToString(ffGeneral, FFloatPreference.ExpPrecision, FFloatPreference.MaxDigits);
    Exit;
  end;
end;

function TwxPropertyItem.Get_Visible: Boolean;
begin
  Result := FVisible;
end;

procedure TwxPropertyItem.Set_OwnerList(const AValue: TwxPropertyItemList);
begin
  FOwnerList := AValue;
end;

procedure TwxPropertyItem.Set_Visible(const AValue: Boolean);
begin
  FVisible := AValue;
end;

function TwxPropertyItemList.IndexOfQualifiedName(AQualifiedName: string): Integer;
var
  I: Integer;
begin
  Result := - 1;

  if Count > 0 then
    for I := 0 to Count - 1 do
      if SameText(Items[I].QualifiedName, AQualifiedName) then Exit(I);
end;

// Cannot be a local procedure in win64
function Compare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PwxPropertyItem(Item1)^.QualifiedName, PwxPropertyItem(Item2)^.QualifiedName);
end;

procedure TwxPropertyItemList.Sort;
begin
  inherited Sort(@Compare);
end;

constructor TwxCanvasStack.Create;
begin
  inherited Create;
  FObjectsStack := TStack<ICanvasObjectsStore>.Create;
end;

destructor TwxCanvasStack.Destroy;
begin
  FObjectsStack.Free;
  inherited;
end;

procedure TwxCanvasStack.Clear;
begin
  inherited Clear;
  FObjectsStack.Clear;
end;

function TwxCanvasStack.Extract: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Extract;
  canvasObjectsStore := FObjectsStack.Extract;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

function TwxCanvasStack.Peek: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Peek;
  canvasObjectsStore := FObjectsStack.Peek;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

function TwxCanvasStack.Pop: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Pop;
  canvasObjectsStore := FObjectsStack.Pop;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

procedure TwxCanvasStack.Push(const ACanvas: TCanvas);
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  inherited Push(ACanvas);
  canvasObjectsStore := TCanvasObjectsStore.Create;
  canvasObjectsStore.SaveCanvasObjects(ACanvas);
  FObjectsStack.Push(canvasObjectsStore);
end;

procedure TwxCanvasStack.TrimExcess;
begin
  inherited TrimExcess;
  FObjectsStack.TrimExcess;
end;

constructor TwxCanvasStack.TCanvasObjectsStore.Create;
begin
  inherited Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
end;

destructor TwxCanvasStack.TCanvasObjectsStore.Destroy;
begin
  FPen.Free;
  FFont.Free;
  FBrush.Free;
  inherited;
end;

procedure TwxCanvasStack.TCanvasObjectsStore.RestoreCanvasObjects(ACanvas: TCanvas);
begin
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Font.Assign(FFont);
  ACanvas.Pen.Assign(FPen);
  ACanvas.Refresh;
end;

procedure TwxCanvasStack.TCanvasObjectsStore.SaveCanvasObjects(ACanvas: TCanvas);
begin
  FBrush.Assign(ACanvas.Brush);
  FFont.Assign(ACanvas.Font);
  FPen.Assign(ACanvas.Pen);
end;

function TwxRttiType.GetUsedProperties: TArray<TRttiProperty>;
var
  prop: TRttiProperty;
  propArray: TArray<TRttiProperty>;
  propArrayLen: Integer;
  {$REGION 'Local Function: Contains'}
  function Contains(APropArray: TArray<TRttiProperty>; AProp: TRttiProperty): Boolean;
  var
    lfProp: TRttiProperty;
  begin
    for lfProp in APropArray do begin
      if lfProp.Name = AProp.Name then Exit(True);
    end;

    Result := False;
  end;
{$ENDREGION}
begin
  Result := GetDeclaredProperties;
  propArray := GetProperties;

  for prop in propArray do begin
    if not Contains(Result, prop) then begin
      propArrayLen := Length(Result) + 1;
      SetLength(Result, propArrayLen);
      Result[propArrayLen - 1] := prop;
    end;
  end;
end;

constructor TwxObjectHost.Create;
begin
  FList := TList<TwxObjectNamePair>.Create;
end;

destructor TwxObjectHost.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TwxObjectHost }
procedure TwxObjectHost.AddObject(AObject: TObject; const AName: string);
var
  objNamePair: TwxObjectNamePair;
begin
  objNamePair.Key := AObject;
  objNamePair.Value := AName;
  FList.Add(objNamePair);
end;

function TwxObjectHost.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TwxObjectHost.GetItem(AIndex: Integer): TwxObjectNamePair;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FList[AIndex];
end;

constructor TwxRecordList<T, PT>.Create;
begin
  ValidateTypeParameters;
  FRecPtrList := TList.Create;
end;

destructor TwxRecordList<T, PT>.Destroy;
begin
  Clear;
  FRecPtrList.Free;
  inherited;
end;

function TwxRecordList<T, PT>.Add(const ARecordValue: T): Integer;
var
  PRec: PByte;
begin
  PtrT(PTToPointer((Add)))^ := ARecordValue;
  Result := Count - 1;
end;

function TwxRecordList<T, PT>.Add: PT;
begin
  Result := PointerToPT(AllocMem(SizeOf(T)));
  FRecPtrList.Add(PTToPointer(Result));
end;

procedure TwxRecordList<T, PT>.Clear;
var
  I: Integer;
  ptr: Pointer;
begin
  for I := 0 to FRecPtrList.Count - 1 do begin
    ptr := FRecPtrList.Items[I];
    FreeRecord(ptr);
  end;

  FRecPtrList.Clear;
end;

procedure TwxRecordList<T, PT>.Delete(const AIndex: Integer);
var
  ptr: Pointer;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  ptr := FRecPtrList.Items[AIndex];
  FreeRecord(ptr);
  FRecPtrList.Delete(AIndex);
end;

function TwxRecordList<T, PT>.First: PT;
begin
  Result := PT(nil);

  if Count > 0 then
    Result := Get_Item(0);
end;

function TwxRecordList<T, PT>.Last: PT;
begin
  Result := PT(nil);

  if Count > 0 then
    Result := Get_Item(Count - 1);
end;

function TwxRecordList<T, PT>.PointerToPT(APointer: Pointer): PT;
var
  temp: PT absolute APointer;
begin
  Result := temp;
end;

// absolute directive: declare a variable that resides at the same address as another variable.
function TwxRecordList<T, PT>.PTToPointer(ARecord: PT): Pointer;
var
  temp: Pointer absolute ARecord;
begin
  Result := temp;
end;

procedure TwxRecordList<T, PT>.Sort(ACompare: TListSortCompare);
begin
  FRecPtrList.Sort(ACompare);
end;

procedure TwxRecordList<T, PT>.FreeRecord(APointer: Pointer);
begin
  if Assigned(APointer) then
  begin
    Finalize(PtrT(APointer)^);
    FreeMem(APointer, SizeOf(T));
  end;
end;

function TwxRecordList<T, PT>.Get_Item(const AIndex: Integer): PT;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := PointerToPT(FRecPtrList.Items[AIndex]);
end;

function TwxRecordList<T, PT>.Get_Count: Integer;
begin
  Result := FRecPtrList.Count;
end;

function TwxRecordList<T, PT>.IndexOf(ARecordPtr: PT): Integer;
begin
  Result := FRecPtrList.IndexOf(PTToPointer(ARecordPtr));
end;

class procedure TwxRecordList<T, PT>.ValidateTypeParameters;
const
  cInvalidTypeParam_1 = 'TwxRecordList<T,PT> type param_1 has invalid type <%s>. It must be a record.';
  cInvalidTypeParam_2 = 'TwxRecordList<T,PT> type param_2 has invalid type <%s>. It must be a pointer to %s.';
var
  cond1, cond2: Boolean;
begin
  cond1 := (SizeOf(PT) <> SizeOf(Pointer));
  cond2 := (PTypeInfo(TypeInfo(PT)).TypeData.RefType <> PTypeInfo(TypeInfo(PtrT)).TypeData.RefType);

  if (PTypeInfo(TypeInfo(T)).Kind <> tkRecord) then
    raise Exception.CreateFmt(cInvalidTypeParam_1, [PTypeInfo(TypeInfo(T)).Name]);

  if (cond1 or cond2) then
    raise Exception.CreateFmt(cInvalidTypeParam_2, [PTypeInfo(TypeInfo(PT)).Name, PTypeInfo(TypeInfo(T)).Name]);
end;

constructor TwxAbstractObjectInspector.Create(AOwner: TComponent);
const
  cDefaultItemHeight = 17;
  cDefaultWidth = 300;
  cDefaultHeight = 300;
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];
  Height := cDefaultHeight;
  Width := cDefaultWidth;

  FBorderStyle := bsSingle;
  FCanvasStack := TwxCanvasStack.Create;
  FCategories := TList<string>.Create;
  FCircularLinkedProperties := TList<string>.Create;
  FComponent := nil;
  FContext := TRttiContext.Create;
  FDefaultCategoryName := ValueManager.DefaultCategoryName;
  FDefaultPropertyValueMap := TDictionary<string, string>.Create;
  FExpandedList := TList<string>.Create;
  FFloatPreference := TwxFloatPreference.Create;
  FItemHeight := cDefaultItemHeight;
  FItems := TwxPropertyItemList.Create;
  FLockUpdate := False;
  FObjectVisibility := mvPublic;
  FOnBeforeAddItem := nil;
  FPropertyCategoryMap := TDictionary<string, Integer>.Create;
  FPropertyInstances := TDictionary<string, TObject>.Create;
  FReadOnly := False;
  FSaveVisibleItems := TList<string>.Create;
  FSortByCategory := False;
  FVisibleItems := TList<PwxPropertyItem>.Create;
end;

destructor TwxAbstractObjectInspector.Destroy;
begin
  FCanvasStack.Free;
  FContext.Free;
  FFloatPreference.Free;

  if Assigned(FExpandedList) then
    FreeAndNil(FExpandedList);

  if Assigned(FPropertyInstances) then
    FreeAndNil(FPropertyInstances);

  if Assigned(FCategories) then
    FreeAndNil(FCategories);

  if Assigned(FPropertyCategoryMap) then
    FreeAndNil(FPropertyCategoryMap);

  if Assigned(FItems) then
    FreeAndNil(FItems);

  if Assigned(FVisibleItems) then
    FreeAndNil(FVisibleItems);

  if Assigned(FSaveVisibleItems) then
    FreeAndNil(FSaveVisibleItems);

  if Assigned(FCircularLinkedProperties) then
    FreeAndNil(FCircularLinkedProperties);

  if Assigned(FDefaultPropertyValueMap) then
    FreeAndNil(FDefaultPropertyValueMap);

  if Assigned(FComponent) and (FComponent is TwxObjectHost) then
    FComponent.Free;

  inherited;
end;

procedure TwxAbstractObjectInspector.BeginUpdate;
begin
  FLockUpdate := True;
end;

procedure TwxAbstractObjectInspector.Changed;
begin
  UpdateProperties(True);
end;

function TwxAbstractObjectInspector.CircularLinkedProperties: TList<string>;
begin
  Result := FCircularLinkedProperties;
end;

procedure TwxAbstractObjectInspector.ClearRegisteredCategorys;
begin
  FCategories.Clear;
  FPropertyCategoryMap.Clear;
  FCategories.Add(FDefaultCategoryName);
  UpdateProperties(True);
end;

procedure TwxAbstractObjectInspector.ComponentChanged;
begin
  // Clear the master category list.
  FCategories.Clear;
  // Add default category name.
  FCategories.Add(FDefaultCategoryName);
  // Clear property to category map
  FPropertyCategoryMap.Clear;
  // Clear default property value map
  FDefaultPropertyValueMap.Clear;
  // Clear circular linked properties
  FCircularLinkedProperties.Clear;
  FItems.Clear;
  FVisibleItems.Clear;
  FSaveVisibleItems.Clear;
  FExpandedList.Clear;

  // Update ComponentClassType.
  if Assigned(FComponent) then
  begin
    FRttiType := FContext.GetType(FComponent.ClassInfo);
    FComponentClassType := TRttiInstanceType(FRttiType).MetaclassType;
  end
  else
  begin
    FComponentClassType := nil;
  end;

  // Fire Changed.
  Changed;
end;

procedure TwxAbstractObjectInspector.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params); // inherited from TWinControl

  if FBorderStyle <> bsNone then
    Params.Style := Params.Style or WS_BORDER;
end;

procedure TwxAbstractObjectInspector.EndUpdate;
begin
  if FLockUpdate then begin
    FLockUpdate := False;
    Invalidate;
  end;
end;

function TwxAbstractObjectInspector.ExpandedList: TList<string>;
begin
  Result := FExpandedList;
end;

procedure TwxAbstractObjectInspector.Invalidate;
begin
  if not FLockUpdate then begin
    inherited;
    CanvasStack.Clear;
    CanvasStack.TrimExcess;
    inherited;
  end;
end;

function TwxAbstractObjectInspector.IsItemCircularLink(AItem: PwxPropertyItem): Boolean;
begin
  Result := FCircularLinkedProperties.Contains(AItem^.QualifiedName);
end;

function TwxAbstractObjectInspector.IsValueNotDefault(AQualifiedName: string; AValue: string): Boolean;
begin
  Result := False;

  if FDefaultPropertyValueMap.ContainsKey(AQualifiedName) then
    Result := FDefaultPropertyValueMap[AQualifiedName] <> AValue;
end;

function TwxAbstractObjectInspector.ItemNeedUpdate(AItem: PwxPropertyItem): Boolean;
  function wasModified(AItemToTest: PwxPropertyItem): Boolean;
  begin
    Result := AItemToTest.IsClass and (FPropertyInstances.ContainsKey(AItemToTest.QualifiedName)) and
      (FPropertyInstances[AItemToTest.QualifiedName] <> AItemToTest.Value.AsObject);
  end;
var
  parentItem: PwxPropertyItem;
begin
  { Item will need update if its instance was changed or the instances of it's parents ! }
  Result := wasModified(AItem);

  if Result then
    Exit;

  parentItem := AItem.Parent;

  while Assigned(parentItem) do begin
    if wasModified(parentItem) then
      Exit(True);

    parentItem := parentItem.Parent;
  end;
end;

function TwxAbstractObjectInspector.ItemOrder(PItem: PwxPropertyItem): Integer;
var
  I: Integer;
  s: string;
begin
  Result := 0;
  s := PItem^.QualifiedName;
  I := Pos('.', s);

  if I <= 0 then
    Exit;

  while I > 0 do begin
    Inc(Result);
    I := Pos('.', s, I + 1);
  end;

  Dec(Result);
end;

function TwxAbstractObjectInspector.LockUpdate: Boolean;
begin
  Result := FLockUpdate;
end;

function TwxAbstractObjectInspector.NeedUpdate: Boolean;
var
  I: Integer;
  PItem: PwxPropertyItem;
begin
  Result := False;

  for I := 0 to FVisibleItems.Count - 1 do begin
    PItem := FVisibleItems[I];

    if ItemNeedUpdate(PItem) then
      Exit(True);
  end;
end;

procedure TwxAbstractObjectInspector.RegisterPropertyInCategory(const ACategoryName, APropertyName: string);
var
  categoryIndex: Integer;
begin
  categoryIndex := FCategories.IndexOf(ACategoryName);

  if categoryIndex < 0 then
    categoryIndex := FCategories.Add(ACategoryName);

  if not FPropertyCategoryMap.ContainsKey(APropertyName) then
    FPropertyCategoryMap.Add(APropertyName, categoryIndex);
end;

function TwxAbstractObjectInspector.SaveVisibleItems: TList<string>;
begin
  Result := FSaveVisibleItems;
end;

procedure TwxAbstractObjectInspector.UpdateVisibleItems;
var
  I: Integer;
  item: PwxPropertyItem;
  visibleCondition: Boolean;

  procedure makeChildrenVisible;
  var
    J: Integer;
  begin
    for J := 0 to item.ChildCount - 1 do item.ChildItems[J].Visible := True;
  end;
begin
  FVisibleItems.Clear;

  for I := 0 to FItems.Count - 1 do begin
    item := FItems.Items[I];

    if item^.IsCategory then begin
      if FExpandedList.Contains(item.QualifiedName) then
        makeChildrenVisible;
    end else begin
      visibleCondition := (item.ChildCount > 0) and FIsSettingComponent and Assigned(OnAutoExpandItemOnInit)
        and OnAutoExpandItemOnInit(Self, item);

      if visibleCondition then
        makeChildrenVisible;
    end;

    if item.Visible or FSaveVisibleItems.Contains(item.QualifiedName) then
      FVisibleItems.Add(item);
  end;
end;

function TwxAbstractObjectInspector.VisibleItems: TList<PwxPropertyItem>;
begin
  Result := FVisibleItems;
end;

procedure TwxCustomControl.WMLButtonDown(var AMessage: TWMLButtonDown);
begin
  FIsMouseLButtonDown := True;
  inherited;
end;

procedure TwxCustomControl.WMLButtonUp(var AMessage: TWMLButtonUp);
begin
  FIsMouseLButtonDown := False;
  inherited;
end;

procedure TwxAbstractObjectInspector.Set_BorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd; // TWinControl.RecreateWnd
  end;
end;

procedure TwxAbstractObjectInspector.Set_Component(AValue: TObject);
begin
  if AValue <> FComponent then begin
    FIsSettingComponent := True;
    try
      if Assigned(FComponent) and (FComponent is TwxObjectHost) then
        FreeAndNil(FComponent);

      FComponent := AValue;
      ComponentChanged;
    finally
      FIsSettingComponent := False;
    end;
  end;
end;

function TwxAbstractObjectInspector.Get_FloatPreference: TwxFloatPreference;
begin
  Result := FFloatPreference;
end;

function TwxCustomControl.Get_IsMouseDown: Boolean;
begin
  Result := FIsMouseLButtonDown;
end;

function TwxCustomControl.Get_IsMouseInControl: Boolean;
var
  screenPos, clientPos: TPoint;
begin
  GetCursorPos(screenPos);
  clientPos := ScreenToClient(screenPos);
  Result := ClientRect.Contains(clientPos);
end;

function TwxCustomControl.Get_IsVclStyleUsed: Boolean;
begin
  Result := StyleServices.Enabled and not StyleServices.IsSystemStyle;
end;

procedure TwxAbstractObjectInspector.Set_ObjectVisibility(const AValue: TMemberVisibility);
begin
  if AValue >= mvPublic then
    FObjectVisibility := AValue
  else
    raise EInvalidObjectVisibility.Create;
end;

procedure TwxAbstractObjectInspector.Set_SortByCategory(const AValue: Boolean);
begin
  if FSortByCategory <> AValue then begin
    FSortByCategory := AValue;
    UpdateProperties(True);
  end;
end;

function TwxCustomControl.Get_UseStyleBorder: Boolean;
begin
  Result := IsVclStyleUsed and (seBorder in StyleElements);
end;

function TwxCustomControl.Get_UseStyleColor: Boolean;
begin
  Result := IsVclStyleUsed and (seClient in StyleElements);
end;

function TwxCustomControl.Get_UseStyleFont: Boolean;
begin
  Result := IsVclStyleUsed and (seFont in StyleElements);
end;

procedure TwxAbstractObjectInspector.UpdateItems;
var
  categories: TList<string>;
  component: TObject;
  componentName: string;
  I: Integer;
  isMultiInstance: Boolean;
  objHost: TwxObjectHost;
  {$REGION 'EnumProps'}
  procedure EnumProps(AInstance: TObject; AParentItem, ACategoryItem: PwxPropertyItem; AQualifiedName, AQualifiedType: string);
  var
    lvAllow: Boolean;
    lvCategoryIndex: Integer;
    lvCategoryItem: PwxPropertyItem;
    lvCategoryName: string;
    lvInstance: TObject;
    lvItem: PwxPropertyItem;
    lvQName, lvQType: string;
    lvRttiProperty: TRttiProperty;
    lvRttiPropertyArray: TArray<TRttiProperty>;
    {$REGION 'AddNewCategory'}
    function AddNewCategory: PwxPropertyItem;
    begin
      Result := FItems.Add;
      Result.AssociatedProperty := nil;
      Result.CategoryIndex := lvCategoryIndex;
      Result.CategoryName := lvCategoryName;
      Result.Component := component;
      Result.FloatPreference := Self.FloatPreference;
      Result.Instance := nil;
      Result.IsCategory := True;
      Result.ObjectVisibility := Self.ObjectVisibility;
      Result.OwnerList := FItems;
      Result.Parent := nil;
      Result.QualifiedName := lvCategoryName + '.';
      Result.SetElementValue := - 1;
      Result.Visible := True;
    end;
    {$ENDREGION}
    {$REGION 'EnumSet'}
    procedure EnumSet;
    var
      llvI: Integer;
      llvRttiType: TRttiType;
      llvSetItem: PwxPropertyItem;
      llvTypeInfo: PTypeInfo;
    begin
      llvTypeInfo := PTypeInfo(lvItem.AssociatedProperty.PropertyType.AsSet.ElementType.Handle);
      llvRttiType := FContext.GetType(llvTypeInfo);
      for llvI := llvRttiType.AsOrdinal.MinValue to llvRttiType.AsOrdinal.MaxValue do
      begin
        llvSetItem := FItems.Add;
        llvSetItem.Component := component;
        llvSetItem.CategoryIndex := - 1;
        llvSetItem.IsCategory := False;
        llvSetItem.OwnerList := FItems;
        llvSetItem.AssociatedProperty := lvRttiProperty;
        llvSetItem.Instance := AInstance;
        llvSetItem.Visible := False;
        llvSetItem.FloatPreference := Self.FloatPreference;
        llvSetItem.ObjectVisibility := Self.ObjectVisibility;
        llvSetItem.Parent := lvItem;
        llvSetItem.SetElementValue := llvI;
        llvSetItem.QualifiedName := lvQName + '.' + IntToStr(llvI);
      end;
    end;
    {$ENDREGION}
  begin
    if not Assigned(AInstance) then
      Exit;

    FRttiType := FContext.GetType(AInstance.ClassInfo);
    lvRttiPropertyArray := TwxRttiType(FRttiType).GetUsedProperties;

    for lvRttiProperty in lvRttiPropertyArray do begin
      if not IsPropVisible(lvRttiProperty, AInstance, FObjectVisibility) then Continue;

      lvAllow := True;
      lvQName := AQualifiedName + '.' + lvRttiProperty.Name;
      lvQType := AQualifiedType + '.' + lvRttiProperty.PropertyType.Tostring;
      lvCategoryIndex := - 1;
      lvCategoryItem := ACategoryItem;

      if (isMultiInstance) and (component <> nil) and (AInstance = component) then begin
        lvCategoryName := componentName;
        if FCategories.Contains(lvCategoryName) then begin
          lvCategoryIndex := FCategories.IndexOf(lvCategoryName)
        end else begin
          lvCategoryIndex := FCategories.Add(lvCategoryName);
          lvCategoryItem := AddNewCategory;
        end;
      end else begin
        if FSortByCategory and (AInstance = component) then begin
          if FPropertyCategoryMap.ContainsKey(lvRttiProperty.Name) then begin
            lvCategoryIndex := FPropertyCategoryMap[lvRttiProperty.Name];
            lvCategoryName := FCategories[lvCategoryIndex];
            lvQName := lvCategoryName + '.' + lvQName;
          end else begin
            lvCategoryName := FDefaultCategoryName;
            lvCategoryIndex := 0;
            lvQName := lvCategoryName + '.' + lvQName;
          end;

          if not categories.Contains(lvCategoryName) then begin
            categories.Add(lvCategoryName);
            lvCategoryItem := AddNewCategory;
          end;
        end;
      end;

      lvItem := FItems.Add;
      lvItem.OwnerList := FItems;

      if (AInstance <> component) then
        lvCategoryIndex := - 1;

      lvItem.Component := component;
      lvItem.CategoryIndex := lvCategoryIndex;
      lvItem.Instance := AInstance;
      lvItem.IsCategory := False;
      lvItem.SetElementValue := - 1;
      lvItem.ObjectVisibility := Self.ObjectVisibility;
      lvItem.FloatPreference := Self.FloatPreference;
      lvItem.Parent := AParentItem;
      lvItem.AssociatedProperty := lvRttiProperty;
      lvItem.QualifiedName := lvQName;

      if FSortByCategory then
        lvItem.Visible := False
      else
        lvItem.Visible := AParentItem = nil;

      if Assigned(FOnBeforeAddItem) then
        lvAllow := FOnBeforeAddItem(Self, lvItem);

      // When it comes to this point, the property must be Published or Public, i.e., filtered by
      // Object Visibility
      if not lvAllow then begin
        FItems.Delete(FItems.Count - 1);
      end else begin
        if lvItem.IsClass then begin
          lvInstance := nil;

          if not lvItem.Value.IsEmpty then
            lvInstance := lvItem.Value.AsObject;

          if Assigned(lvInstance) then begin
            if not IsCircularLink(lvInstance.ClassInfo, lvQType, component, Self.ObjectVisibility) then
              EnumProps(lvInstance, lvItem, lvCategoryItem, lvItem.QualifiedName, lvQType)
            else
              FCircularLinkedProperties.Add(lvQName);
          end;

          FPropertyInstances.Add(lvQName, lvInstance);
        end;

        if (lvItem^.IsSet) then
          EnumSet;
      end;
    end;
  end;
{$ENDREGION}
begin
  FItems.Clear;
  FCircularLinkedProperties.Clear;
  FPropertyInstances.Clear;

  if not Assigned(FComponent) then
    Exit;

  categories := TList<string>.Create;

  try
    isMultiInstance := False;
    component := FComponent;

    if FComponent is TwxObjectHost then begin
      objHost := TwxObjectHost(FComponent);
      FSortByCategory := True;
      isMultiInstance := True;
      FCategories.Clear;
      FPropertyCategoryMap.Clear;

      for I := 0 to objHost.Count - 1 do begin
        component := objHost.Item[I].Key;
        componentName := objHost.Item[I].Value;
        EnumProps(component, nil, nil, componentName + '.' + component.Tostring, component.Tostring);
      end;
    end else begin
      EnumProps(FComponent, nil, nil, FComponent.Tostring, FComponent.Tostring);
    end;

    FItems.Sort;
  finally
    FreeAndNil(categories);
  end
end;

{ TzObjInspectorSizing }
constructor TwxSplitteredObjectInspector.Create(AOwner: TComponent);
const
  cDefaultSplitterPos = 100;
begin
  inherited;
  FOnSplitterPosChanged := nil;
  Color := clWhite; // TControl.Color
  FFixedSplitter := False;
  FSplitterDown := False;
  FSplitterColor := clGray;
  FSplitterPos := cDefaultSplitterPos;
end;

destructor TwxSplitteredObjectInspector.Destroy;
begin
  inherited;
end;

procedure TwxSplitteredObjectInspector.DrawSplitter(ACanvas: TCanvas);
var
  color: TColor;
begin
  ACanvas.Refresh;
  color := FSplitterColor;

  if UseStyleColor then
    color := StyleServices.GetStyleColor(scSplitter);

  ACanvas.Pen.Color := color;
  ACanvas.MoveTo(FSplitterPos, 0);
  ACanvas.LineTo(FSplitterPos, Height);
end;

procedure TwxSplitteredObjectInspector.InvalidateNC;
begin
  // The WM_NCPAINT message is sent to a window when its frame must be painted.
  if HandleAllocated and not LockUpdate then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TwxSplitteredObjectInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;

  if csDesigning in ComponentState then
    Exit;

  FSplitterDown := False;

  if FFixedSplitter then
    Exit;

  GetCursorPos(pt); //user32 Win32 API
  pt := ScreenToClient(pt); // TControl.ScreenToClient
  pt.Y := 0;

  if SplitterRect.Contains(pt) then
    FSplitterDown := True;
end;

procedure TwxSplitteredObjectInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if csDesigning in ComponentState then
    Exit;

  FSplitterDown := False;
end;

procedure TwxSplitteredObjectInspector.Paint;
begin
  inherited;
  DrawSplitter(Canvas);
end;

procedure TwxSplitteredObjectInspector.SplitterPosChanged(var ANewPos: Integer);
begin
  if Assigned(FOnSplitterPosChanged) then
    FOnSplitterPosChanged(Self, ANewPos);
end;

procedure TwxSplitteredObjectInspector.WMMouseMove(var AMessage: TWMMouseMove);
const
  cSplitterPosMargin = 10;
var
  pt: TPoint;
begin
  inherited;

  if (csDesigning in ComponentState) or (FFixedSplitter) then
    Exit;

  pt.X := AMessage.XPos;
  pt.Y := AMessage.YPos;

  if SplitterRect.Contains(pt) then
    Cursor := crHSplit
  else
    Cursor := crArrow;

  if FSplitterDown then begin
    if (pt.X <> FSplitterPos) and (pt.X > cSplitterPosMargin ) and (pt.X < ClientWidth - cSplitterPosMargin) then begin
      FSplitterPos := pt.X;
      SplitterPosChanged(FSplitterPos);
      Invalidate;
    end;
  end;
end;

procedure TwxSplitteredObjectInspector.Set_SplitterColor(const AValue: TColor);
begin
  if AValue <> FSplitterColor then begin
    FSplitterColor := AValue;
    Invalidate;
  end;
end;

procedure TwxSplitteredObjectInspector.Set_SplitterPos(const AValue: Integer);
const
  cSplitterPosMargin = 10;
begin
  if (FSplitterPos <> AValue) and (AValue > cSplitterPosMargin) and (AValue < ClientWidth - cSplitterPosMargin) then begin
    FSplitterPos := AValue;
    Invalidate;
  end;
end;

function TwxSplitteredObjectInspector.Get_SplitterRect: TRect;
const
  cSplitterRectMargin = 5;
begin
  Result := Rect(
    FSplitterPos - cSplitterRectMargin,
    0,
    FSplitterPos + cSplitterRectMargin,
    Height
  );
end;

constructor TwxHeaderedObjectInspector.Create(AOwner: TComponent);
begin
  inherited;
  FOnHeaderMouseDown := nil;
  FHeaderPressed := False;
  FHeaderPropPressed := False;
  FHeaderValuePressed := False;
  FHeaderPropText := ValueManager.DefaultHeaderPropText;;
  FHeaderValueText := ValueManager.DefaultHeaderValueText;
  FShowHeader := False;
end;

procedure TwxHeaderedObjectInspector.Paint;
begin
  if FShowHeader then PaintHeader;
  inherited;
end;

procedure TwxHeaderedObjectInspector.PaintHeader;
var
  themedElemDetails: TThemedElementDetails;
  rect: TRect;
begin
  CanvasStack.Push(Canvas);

  try
    // Draw HeaderProp
    if FHeaderPropPressed then
      themedElemDetails := StyleServices.GetElementDetails(thHeaderItemPressed)
    else
      themedElemDetails := StyleServices.GetElementDetails(thHeaderItemNormal);;

    StyleServices.DrawElement(Canvas.Handle, themedElemDetails, HeaderPropRect);

    rect := HeaderPropRect;
    Inc(rect.Left, 10);
    StyleServices.DrawText(Canvas.Handle, themedElemDetails, FHeaderPropText, rect, [tfLeft, tfSingleLine, tfVerticalCenter]);

    // Draw HeaderValue
    if FHeaderValuePressed then
      themedElemDetails := StyleServices.GetElementDetails(thHeaderItemPressed)
    else
      themedElemDetails := StyleServices.GetElementDetails(thHeaderItemNormal);;

    StyleServices.DrawElement(Canvas.Handle, themedElemDetails, HeaderValueRect);

    rect := HeaderValueRect;
    Inc(rect.Left, 10);
    StyleServices.DrawText(Canvas.Handle, themedElemDetails, FHeaderValueText, rect, [tfLeft, tfSingleLine, tfVerticalCenter]);
  finally
    CanvasStack.Pop;
  end;
end;

procedure TwxHeaderedObjectInspector.WMLButtonDown(var AMessage: TWMLButtonDown);
var
  pt: TPoint;
begin
  inherited;

  if not FShowHeader then
    Exit;

  pt := Point(AMessage.XPos, AMessage.YPos);

  if SplitterRect.Contains(pt) then
    Exit;

  if HeaderRect.Contains(pt) then begin
    if not FHeaderPressed then begin
      FHeaderPropPressed := HeaderPropRect.Contains(pt);
      FHeaderValuePressed := HeaderValueRect.Contains(pt);
      FHeaderPressed := True;
      Invalidate;
    end;

    if Assigned(FOnHeaderMouseDown) then begin
      if FHeaderPropPressed then begin
        FOnHeaderMouseDown(Self, hpLeft, pt.X, pt.Y)
      end else begin
        if FHeaderValuePressed then
          FOnHeaderMouseDown(Self, hpRight, pt.X, pt.Y);
      end;
    end;
  end;
end;

procedure TwxHeaderedObjectInspector.WMLButtonUp(var AMessage: TWMLButtonUp);
var
  pt: TPoint;
begin
  inherited;

  if not FShowHeader then
    Exit;

  pt := Point(AMessage.XPos, AMessage.YPos);

  if HeaderRect.Contains(pt) then begin
    if FHeaderPressed then begin
      FHeaderPropPressed := False;
      FHeaderValuePressed := False;
      FHeaderPressed := False;
      Invalidate;
    end;
  end;
end;

function TwxHeaderedObjectInspector.Get_HeaderPropRect: TRect;
begin
  Result := Rect(0, 0, SplitterPos, HeaderRect.Height);
end;

procedure TwxHeaderedObjectInspector.Set_HeaderPropText(const AValue: string);
begin
  if FHeaderPropText <> AValue then begin
    FHeaderPropText := AValue;
    Invalidate;
  end;
end;

function TwxHeaderedObjectInspector.Get_HeaderRect: TRect;
begin
  Result := Rect(0, 0, Width, ItemHeight + (ItemHeight div 2));
end;

function TwxHeaderedObjectInspector.Get_HeaderValueRect: TRect;
begin
  Result := Rect(SplitterPos, 0, Width, HeaderRect.Height);
end;

procedure TwxHeaderedObjectInspector.Set_HeaderValueText(const AValue: string);
begin
  if FHeaderValueText <> AValue then begin
    FHeaderValueText := AValue;
    Invalidate;
  end;
end;

procedure TwxHeaderedObjectInspector.Set_ShowHeader(const AValue: Boolean);
begin
  if FShowHeader <> AValue then begin
    FShowHeader := AValue;
    Invalidate;
  end;
end;

constructor TwxScrollableObjectInspector.Create(AOwner: TComponent);
begin
  inherited;
  FPrevScrollPos := 0;
end;

destructor TwxScrollableObjectInspector.Destroy;
begin

  inherited;
end;

procedure TwxScrollableObjectInspector.CMFontChanged(var AMessage: TMessage);
const
  cDummyText = 'WA';
  cItemHeightMargin = 4;
begin
  inherited;
  Canvas.Font.Assign(Font);
  ItemHeight := Canvas.TextHeight(cDummyText) + cItemHeightMargin; // 17;
end;

procedure TwxScrollableObjectInspector.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  AParams.Style := AParams.Style or WS_VSCROLL;
end;

function TwxScrollableObjectInspector.FirstItemIndex: Integer;
begin
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_POS;
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  Result := Max(0, FScrollInfo.nPos);
end;

function TwxScrollableObjectInspector.IndexFromPoint(APoint: TPoint): Integer;
var
  R: TRect;
  I, Y: Integer;
begin
  Result := - 1;

  for I := 0 to MaxItemCount do begin
    Y := ItemTop(I);
    R := Rect(0, Y, Width, Y + ItemHeight);

    if R.Contains(APoint) then begin
      Result := (I + FirstItemIndex);

      if (Result >= VisibleItems.Count) then
        Exit(-1)
      else
        Exit(Result);
    end;
  end;
end;

function TwxScrollableObjectInspector.ItemTop(AIndex: Integer): Integer;
begin
  Result := AIndex * ItemHeight;

  if ShowHeader then
    Inc(Result, HeaderRect.Height);
end;

function TwxScrollableObjectInspector.LastItemIndex: Integer;
begin
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_POS;
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  Result := min(VisiblePropCount - 1, FScrollInfo.nPos + MaxItemCount);
end;

function TwxScrollableObjectInspector.MaxItemCount: Integer;
var
  LHeight: Integer;
begin
  LHeight := Height;

  if ShowHeader then
    Dec(LHeight, HeaderRect.Height);

  Result := LHeight div ItemHeight;
end;

function TwxScrollableObjectInspector.IndexToVirtualIndex(AIndex: Integer): Integer;
begin
  Result := AIndex - FirstItemIndex;
end;

procedure TwxScrollableObjectInspector.Paint;
var
  I: Integer;
  FirstItem: Integer;
  LastItem: Integer;
begin
  PaintBackground(Canvas);
  FirstItem := FirstItemIndex;
  LastItem := LastItemIndex;

  for I := FirstItem to LastItem do begin
    CanvasStack.Push(Canvas);
    PaintItem(i);
    CanvasStack.Pop;
  end;

  inherited;
end;

procedure TwxScrollableObjectInspector.PaintBackground(ACanvas: TCanvas);
var
  LColor: TColor;
begin
  ACanvas.Refresh;
  LColor := Color;

  if UseStyleColor then
    LColor := StyleServices.GetStyleColor(scWindow);

  ACanvas.Brush.Color := LColor;
  ACanvas.FillRect(ClientRect);
end;

procedure TwxScrollableObjectInspector.PaintItem(AIndex: Integer);
begin
  { ==> Override <== }
end;

procedure TwxScrollableObjectInspector.UpdateScrollBar;
begin
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  FScrollInfo.nMin := 0;
  FScrollInfo.nMax := VisiblePropCount - 1;
  FScrollInfo.nPage := MaxItemCount;

  if Assigned(Parent) and not (csDestroying in ComponentState) then begin
    SetScrollInfo(Handle, SB_VERT, FScrollInfo, False);
    InvalidateNC;
  end;
end;

procedure TwxScrollableObjectInspector.WMEraseBkgnd(var AMessage: TWMEraseBkgnd);
begin
  { Background will be painted on the WM_PAINT event ! }
  AMessage.Result := 1;
end;

{Besides WM_GETDLGCODE message, we can also use CM_WANTSPECIALKEY, which is more flexible than the
former. When the control receives any "special" key, it will send a CM_WANTSPECIALKEY message to itself.
The "special" keys include: VK_TAB VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_RETURN VK_EXECUTE VK_ESCAPE  and VK_CANCEL
If the return value is non-zero, this key will send to KeyPress, otherwise it will be procesed by
the control's parent default handler.

TMyComponent = class (TWinControl)
protected
  procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
end;

procedure TMyComponent.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if Message.CharCode = VK_LEFT then // Handles LEFT arrow only.
    Message.Result := 1;
end;
}
procedure TwxScrollableObjectInspector.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  AMessage.Result := DLGC_WANTARROWS;
end;

procedure TwxScrollableObjectInspector.WMSize(var AMessage: TWMSize);
begin
  inherited;
  UpdateScrollBar;
end;

procedure TwxScrollableObjectInspector.WMVScroll(var AMessage: TWMVScroll);
var
  YPos: Integer;
  P: TPoint;
  Cntrl: TControl;
  procedure DoScrollWindow(XAmount, YAmount: Integer);
  var
    LScrollArea: TRect;
    hrgnUpdate: HRGN;
    ScrollFlags: UINT;
    procedure VertScrollChilds(pExcludeRect: PRect);
    var
      I: Integer;
      L: Integer;
      Child: TControl;
    begin
      { Manually Scroll Childs when ScrollFlags <> SW_SCROLLCHILDREN ! }
      for I := 0 to Self.ControlCount - 1 do begin
        Child := Self.Controls[i];

        if not (Child is TwxObjectInspectorButton) and (Child.Visible) then begin
          L := Child.Top + YAmount;

          if Assigned(pExcludeRect) and pExcludeRect.Contains(Point(Child.Left, L)) then begin
            if YAmount < 0 then
              Dec(L, pExcludeRect.Height)
            else
              Inc(L, pExcludeRect.Height)
          end;

          Child.Top := L;
          Child.Update;
        end;
      end;
    end;

  begin
    { If the header is visible
      => We must exclude the header area from being scrolled ! }
    if ShowHeader then begin
      UpdateWindow(Handle);
      LScrollArea := Rect(0, HeaderRect.Height, ClientWidth, ClientHeight);
      ScrollFlags := SW_INVALIDATE or SW_SCROLLCHILDREN;
      { Set the area that will be updated by the ScrollWindowEx function ! }
     // with LScrollArea do
      hrgnUpdate := CreateRectRgn(LScrollArea.Left, LScrollArea.Top, LScrollArea.Right, LScrollArea.Bottom);
      ScrollWindowEx(Handle, XAmount, YAmount, nil, @LScrollArea, hrgnUpdate, nil, ScrollFlags);
      DeleteObject(hrgnUpdate);
    end else begin
      ScrollWindow(Handle, XAmount, YAmount, nil, nil);
      UpdateWindow(Handle); { Update the non validated area . }
    end;
    {
      ScrollFlags := SW_INVALIDATE ;
      if Assigned(pHeaderRect) then
      VertScrollChilds(pHeaderRect); // Manually Scroll Childs .
    }
  end;
begin
  { if the header is visible and the Edit is visible,
    the Edit caret will be painted over the header.
    we need first to hide the caret before scrolling .
  }
  if ShowHeader and (ContainsWindow(GetCaretWnd, Handle)) and IsCaretVisible then
    HideCaret(0);

  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  YPos := FScrollInfo.nPos;

  case AMessage.ScrollCode of
    SB_TOP:
      FScrollInfo.nPos := FScrollInfo.nMin;
    SB_BOTTOM:
      FScrollInfo.nPos := FScrollInfo.nMax;
    SB_LINEUP:
      FScrollInfo.nPos := FScrollInfo.nPos - 1;
    SB_LINEDOWN:
      FScrollInfo.nPos := FScrollInfo.nPos + 1;
    SB_PAGEUP:
      FScrollInfo.nPos := FScrollInfo.nPos - Integer(FScrollInfo.nPage);
    SB_PAGEDOWN:
      FScrollInfo.nPos := FScrollInfo.nPos + Integer(FScrollInfo.nPage);
    SB_THUMBTRACK: { VCL Style Support ! }
      begin
        if StyleServices.Available and (not StyleServices.IsSystemStyle) then begin
          {
            if the Vcl Style is enabled and the user is scrolling ,
            the (ScrollInfo.nPos - ScrollInfo.nTrackPos) is always 0 !
            => We need to scroll manually !
          }
          DoScrollWindow(0, ItemHeight * (FPrevScrollPos - FScrollInfo.nPos));
          FPrevScrollPos := AMessage.Pos;
          Invalidate;
          Exit; // must exit
        end else begin
          FScrollInfo.nPos := FScrollInfo.nTrackPos;
        end;
      end;
    SB_ENDSCROLL:
      begin
        { Restore caret visibility ! }
        if ShowHeader and (ContainsWindow(GetCaretWnd, Handle)) and not IsCaretVisible then begin
          Cntrl := GetCaretControl;
          if Assigned(Cntrl) then begin
            GetCaretPos(P);
            P := Cntrl.ClientToParent(P);
            if not HeaderRect.Contains(P) then ShowCaret(0);
          end;
        end;
      end;
  end;

  FScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, FScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);

  { If the position has changed, scroll window and Update it. }
  if (FScrollInfo.nPos <> YPos) then
    DoScrollWindow(0, ItemHeight * (YPos - FScrollInfo.nPos));
  { Save the current pos }

  FPrevScrollPos := FScrollInfo.nPos;
end;

procedure TwxScrollableObjectInspector.WMWindowPosChanged(var AMessage: TWMWindowPosChanged);
begin
  inherited;
  UpdateScrollBar;
end;

function TwxScrollableObjectInspector.Get_VisiblePropCount: Integer;
begin
  Result := VisibleItems.Count;
end;

constructor TwxCustomObjectInspector.Create(AOwner: TComponent);
begin
  inherited;

  if csDesigning in ComponentState then
    Component := Self;

  FUnRegisterKeys := False;
  FAllowSearch := True;
  FShowItemHint := True;
  FIsItemHint := False;
  FBoldHint := False;
  FPropsNeedHint := False;
  FValuesNeedHint := False;
  FOnGetItemReadOnly := nil;
  FOnItemSetValue := nil;
  FOnExpandItem := nil;
  FOnCollapseItem := nil;
  FOnSelectItem := nil;
  FSelItem := TwxPropertyItem.Empty;
  ParentBackground := False;
  DoubleBuffered := True;
  FShowGutter := True;
  FGutterWidth := 12;
  FNameColor := clBtnText;
  FGutterColor := clCream;
  FGutterEdgeColor := clGray;
  FReadOnlyColor := clGrayText;
  FHighlightColor := RGB(224, 224, 224);
  FReferencesColor := clMaroon;
  FSubPropertiesColor := clGreen;
  FValueColor := clNavy;
  FCategoryColor := $00E0E0E0;
  FCategoryTextColor := $00400080;
  FNonDefaultValueColor := clNavy;
  FBoldNonDefaultValue := True;
  FShowGridLines := False;
  FGridColor := clBlack;
  FAutoCompleteText := True;
  FTrackChange := False;
  FSepTxtDis := 4;
  FSelectedIndex := - 1;
  FPropInspEdit := TwxObjectInspectorEdit.Create(Self);
  FPropInspEdit.Visible := False;

  if not (csDesigning In ComponentState) then
    FPropInspEdit.Parent := Self;

  FPropInspEdit.BorderStyle := bsNone;
end;

destructor TwxCustomObjectInspector.Destroy;
begin
  inherited;
end;

{ TzCustomObjInspector }

function TwxCustomObjectInspector.CanDrawChevron(Index: Integer): Boolean;
var
  PItem: PwxPropertyItem;
  iOrd: Integer;
begin
  Result := False;

  if (Index > - 1) and (Index = FSelectedIndex) then begin
    PItem := VisibleItems.Items[Index];
    iOrd := ItemOrder(PItem);

    if (iOrd > 0) or ((iOrd = 0) and (not PItem.HasChild)) then
      Exit(True)
    else
      Exit(False);
  end;
end;

procedure TwxCustomObjectInspector.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if (M <> D) then begin
    FGutterWidth := MulDiv(FGutterWidth, M, D);
    SplitterPos := MulDiv(SplitterPos, M, D);

    ValueManager.MinimumPlusSignWidth := MulDiv(10, M, D);
    ValueManager.ScaledColorRectWidth := MulDiv(ValueManager.DefaultColorRectWidth, M, D);

    TwxObjectInspectorButton.ScaledWidth     := MulDiv(TwxObjectInspectorButton.DefaultWidth, M, D);
    TwxObjectInspectorButton.ScaledArrowSize := MulDiv(TwxObjectInspectorButton.DefaultArrowSize, M, D);

    FSepTxtDis := MulDiv(FSepTxtDis, M, D);
  end;

  inherited ChangeScale(M, D, isDpiChange);
end;

procedure TwxCustomObjectInspector.CMHintShow(var AMessage: TCMHintShow);
begin
  if FIsItemHint and FShowItemHint then begin
    AMessage.HintInfo.HintPos := FHintPoint;
    AMessage.HintInfo.HintWindowClass := TzObjectInspectorItemHintWindow;
    AMessage.HintInfo.HintData := Pointer(FBoldHint);
  end else begin
    inherited;
  end;

  FIsItemHint := False;
end;

procedure TwxCustomObjectInspector.CMStyleChanged(var AMessage: TMessage);
begin
  inherited;
  FSelectedIndex := - 1;
  UpdateScrollBar;
  UpdateEditControl(False);
end;

procedure TwxCustomObjectInspector.CollapseAll;
begin
  SaveVisibleItems.Clear;
  ExpandedList.Clear;
  UpdateProperties(True);
end;

function TwxCustomObjectInspector.CollapseItem(APropItem: PwxPropertyItem): Boolean;
begin
  Result := DoCollapseItem(APropItem);

  if Result then
    UpdateProperties(True);
end;

procedure TwxCustomObjectInspector.CreateWnd;
begin
  inherited;
  FSelectedIndex := - 1;
end;

function TwxCustomObjectInspector.DoCollapseItem(APropItem: PwxPropertyItem): Boolean;
var
  I: Integer;
  PChild: PwxPropertyItem;
begin
  Result := APropItem^.HasChild;

  if not Result then
    Exit;

  if Assigned(FOnCollapseItem) then if not FOnCollapseItem(Self, APropItem) then
    Exit(False);

  Result := False; // Indicate that item is already Collapsed !

  for I := 0 to APropItem.ChildCount - 1 do begin
    PChild := APropItem.ChildItems[i];

    if Assigned(PChild) then begin

      if PChild^.Visible then begin

        if PChild^.HasChild then
          DoCollapseItem(PChild);

        if FPropInspEdit.PropertyItem = PChild then
          FPropInspEdit.Visible := False;

        Result := True;
      end;

      PChild.Visible := False;

      if SaveVisibleItems.Contains(PChild^.QualifiedName) then
        SaveVisibleItems.Remove(PChild^.QualifiedName);
    end;
  end;
  // PItem.FVisible := False;
  // if FSaveVisibleItems.Contains(PItem.QualifiedName) then
  // FSaveVisibleItems.Remove(PItem.QualifiedName);
end;

function TwxCustomObjectInspector.DoExpandItem(APropItem: PwxPropertyItem): Boolean;
var
  I: Integer;
  PChild: PwxPropertyItem;
  procedure MakeChildsVisible(PParent: PwxPropertyItem; Visible: Boolean);
  var
    J: Integer;
    P: PwxPropertyItem;
  begin
    for J := 0 to PParent.ChildCount - 1 do begin
      P := PParent^.ChildItems[J];
      P^.Visible := Visible;

      if ExpandedList.Contains(P^.QualifiedName) then
        MakeChildsVisible(P, True);

      if not SaveVisibleItems.Contains(P^.QualifiedName) then
        SaveVisibleItems.Add(P^.QualifiedName);
    end;
  end;

begin
  Result := APropItem.HasChild;

  if not Result then
    Exit;

  if CircularLinkedProperties.Contains(APropItem.QualifiedName) then
    Exit(False);

  if Assigned(FOnExpandItem) then if not FOnExpandItem(Self, APropItem) then
    Exit(False);

  Result := False; // Indicate that item is already Expanded !

  if not ExpandedList.Contains(APropItem^.QualifiedName) then
    ExpandedList.Add(APropItem^.QualifiedName);

  if not SaveVisibleItems.Contains(APropItem^.QualifiedName) then
    SaveVisibleItems.Add(APropItem^.QualifiedName);

  for I := 0 to APropItem^.ChildCount - 1 do begin
    PChild := APropItem^.ChildItems[i];

    if Assigned(PChild) then begin

      if not PChild^.Visible then
        Result := True;

      PChild^.Visible := True;

      if ExpandedList.Contains(PChild^.QualifiedName) then
        MakeChildsVisible(PChild, True);

      if not SaveVisibleItems.Contains(PChild^.QualifiedName) then
        SaveVisibleItems.Add(PChild^.QualifiedName);
    end;
  end;
end;

procedure TwxCustomObjectInspector.DoExtraRectClick;
var
  PItem: PwxPropertyItem;
  Value: TValue;
begin
  if ReadOnly then
    Exit;

  PItem := VisibleItems[FExtraRectIndex];

  if not Assigned(PItem) then
    Exit;

  if Assigned(FOnGetItemReadOnly) then
    if FOnGetItemReadOnly(Self, PItem) then
      Exit;

  Value := ValueManager.GetExtraRectResultValue(PItem);
  DoSetValue(PItem, Value);
end;

function TwxCustomObjectInspector.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  M: TWMVScroll;
begin
  if Assigned(FPropInspEdit.ListBox) and IsWindowVisible(FPropInspEdit.ListBox.Handle) then
    Exit(False);

  M := Default (TWMVScroll);
  M.ScrollCode := SB_LINEDOWN;
  WMVScroll(M);
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TwxCustomObjectInspector.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  M: TWMVScroll;
begin
  if Assigned(FPropInspEdit.ListBox) and IsWindowVisible(FPropInspEdit.ListBox.Handle) then
    Exit(False);

  M := Default (TWMVScroll);
  M.ScrollCode := SB_LINEUP;
  WMVScroll(M);
  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

function TwxCustomObjectInspector.DoSelectCaret(AIndex: Integer): Boolean;
var
  x, y, I, offset: Integer;
  s: string;
begin
  Result := False;

  if (AIndex > - 1) and (AIndex < VisibleItems.Count - 1) then begin
    if CanFocus then begin
      SelectItem(AIndex);

      if GetFocus <> Handle then
        SetFocus;

      x := PropTextRect[AIndex].Left;
      y := ItemRect[AIndex].Top;
      s := FSearchText;
      offset := 0;

      if not s.IsEmpty then begin
        for I := 1 to Length(s) do offset := offset + Canvas.TextWidth(s[i]); { Calc caret pos }
      end;

      SetCaretPos(x + offset - 1, y + 1);
      ShowCaret(Handle);
      Result := True;
    end;
  end;
end;

function TwxCustomObjectInspector.DoSetValue(APropItem: PwxPropertyItem; var Value: TValue): Boolean;
begin
  Result := Assigned(APropItem);

  if not Result then
    Exit;

  if Assigned(FOnItemSetValue) then
    Result := FOnItemSetValue(Self, APropItem, Value);

  if Result then begin
    ValueManager.SetValue(APropItem, Value);

    if APropItem.IsClass then
      UpdateProperties(); { Must rebuild the list . }
  end;

  FPropInspEdit.UpdateEditText; // required on Result is True or False
  Invalidate;
end;

procedure TwxCustomObjectInspector.ExpandAll;
var
  I: Integer;
begin
  ExpandedList.Clear;
  SaveVisibleItems.Clear;
  UpdateItems;

  for I := 0 to Items.Count - 1 do
    Items.Items[i].Visible := True;

  UpdateVisibleItems;
  UpdateScrollBar;
  UpdateEditControl(False);
  Invalidate;
end;

function TwxCustomObjectInspector.ExpandItem(APropItem: PwxPropertyItem): Boolean;
begin
  Result := DoExpandItem(APropItem);

  if Result then
    UpdateProperties(True);
end;

procedure TwxCustomObjectInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  LSelectedItem: PwxPropertyItem;
  LTxt: string;
  I: Integer;
  PItem: PwxPropertyItem;
  NewIndex: Integer;
begin
  inherited;

  if not FAllowSearch then
    Exit;

  LSelectedItem := SelectedItem;

  if (GetCaretWnd = Handle) and (FSelectedIndex > - 1) and Assigned(LSelectedItem) then begin
    NewIndex := FSelectedIndex;
    case Key of
      vkUp:
        begin
          Dec(NewIndex);
          NewIndex := max(0, NewIndex);
        end;
      vkDown:
        begin
          Inc(NewIndex);
          NewIndex := min(VisiblePropCount - 1, NewIndex);
        end;
    else
      NewIndex := - 1;
      LTxt := VirtualKeyToStr(Key);

      if LTxt.IsEmpty then begin
        FSearchText := '';
        Exit;
      end;

      FSearchText := FSearchText + LTxt;

      for I := 0 to VisibleItems.Count - 1 do begin
        PItem := VisibleItems[i];

        if PItem.Parent = LSelectedItem.Parent then begin
          if StartsWith(FSearchText, PItem.Name) then begin
            FSearchText := Copy(PItem.Name, 0, Length(FSearchText)); { Respect the case sensitive }
            if DoSelectCaret(i) then
              Exit;
          end;
        end;
      end;
    end;

    if NewIndex >= 0 then begin
      FSearchText := '';
      SelectItem(NewIndex);
    end;
  end;

  FSearchText := '';
end;

procedure TwxCustomObjectInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  P: TPoint;
  PItem: PwxPropertyItem;
  MustUpdate: Boolean;
  SaveItem: TwxPropertyItem;
begin
  inherited;
  if CanFocus then WinApi.Windows.SetFocus(Handle);
  FSearchText := '';
  FExtraRectIndex := - 1;
  P := Point(X, Y);

  if SplitterRect.Contains(P) then
    Exit;

  if ShowHeader and HeaderRect.Contains(P) then
    Exit;

  if FTrackChange and NeedUpdate then
    UpdateProperties(); // In case that some property was changed outside of the Inspector !

  MustUpdate := False; // No need to Update .
  Index := IndexFromPoint(P);

  if Index > - 1 then begin
    PItem := VisibleItems.Items[Index];

    if not FTrackChange then begin
      if ItemNeedUpdate(PItem) then begin
        SaveItem := PItem^;
        UpdateProperties;
        Index := Items.IndexOfQualifiedName(SaveItem.QualifiedName);

        if Index < 0 then begin
          Invalidate;
          Exit;
        end;

        PItem := Items.Items[Index];
        Index := VisibleItems.IndexOf(PItem);
        PItem := VisibleItems.Items[Index];
      end;
    end;

    if (PItem^.HasChild) and (PlusMinBtnRect[Index].Contains(P)) and (not IsItemCircularLink(PItem)) then begin
      if PItem^.ChildCount = 0 then begin
        { FItems list does not contains childs ! }
        { => Must ReBuild the list ! }
        SaveItem := PItem^;
        UpdateItems;
        UpdateVisibleItems;
        Index := Items.IndexOfQualifiedName(SaveItem.QualifiedName);

        if Index < 0 then
          Exit;

        PItem := Items.Items[Index];
        Index := VisibleItems.IndexOf(PItem);

        if Index < 0 then
          Exit;
      end;

      if PItem.Expanded then begin
        if ExpandedList.Contains(PItem^.QualifiedName) then
          ExpandedList.Remove(PItem^.QualifiedName);

        MustUpdate := DoCollapseItem(PItem);
      end else begin
        MustUpdate := DoExpandItem(PItem);
      end;
    end;

    if MustUpdate then begin
      // UpdateProperties(False);
      UpdateVisibleItems;
      UpdateScrollBar;

      if Index = FSelectedIndex then
        Invalidate; // SelectItem will not Invalidate!
    end;

    FClickTime := GetTickCount;
    SelectItem(Index);

    if PItem.IsCategory then
      Exit;
  end;

  if Index > - 1 then begin
    if not ExtraRect[Index].IsEmpty then begin
      if ExtraRect[Index].Contains(P) then
        FExtraRectIndex := Index;
    end;
  end;
end;

procedure TwxCustomObjectInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  w: Integer;
  P: TPoint;
  R: TRect;
  PItem: PwxPropertyItem;
  MustShow: Boolean;
begin
  inherited;

  if not FShowItemHint then
    Exit;

  P := Point(X, Y);
  Index := IndexFromPoint(P);

  if (Index > - 1) and (FPrevHintIndex <> Index) and (FPropsNeedHint or FValuesNeedHint) then begin
    { Avoid this loop when there is no reason !
      ==> (FPropsNeedHint or FValuesNeedHint) must be True
      in order to show hint .
      ==> Check PaintItem routines !
    }
    ShowHint := False;
    FPrevHintIndex := Index;
    PItem := VisibleItems[Index];

    if PItem^.IsCategory then
      Exit;

    R := ValueRect[Index];
    FIsItemHint := True;

    if R.Contains(P) then begin
      FBoldHint := False;
      R := ValueTextRect[Index];
      Hint := PItem.ValueAsstring;
      w := Canvas.TextWidth(Hint);
      MustShow := R.Width <= w;
    end else begin
      R := PropTextRect[Index];
      Hint := PItem.Name;
      FBoldHint := IsValueNotDefault(PItem^.QualifiedName, PItem^.ValueAsstring);
      w := Canvas.TextWidth(Hint);
      MustShow := R.Width <= w;
    end;

    ShowHint := MustShow;

    if ShowHint then begin
      P.X := R.Left;
      P.Y := R.Top;
      P := ClientToScreen(P);
      FHintPoint := P;
    end;
  end;
end;

procedure TwxCustomObjectInspector.Paint;
var
  I: Integer;
  PItem: PwxPropertyItem;
  FirstIndex, LastIndex: Integer;
begin
  inherited;
  FirstIndex := FirstItemIndex;
  LastIndex := LastItemIndex;

  for I := FirstIndex to LastIndex do begin
    PItem := VisibleItems[I];
    if PItem^.IsCategory then
      PaintCategory(I);
  end;

  CanvasStack.TrimExcess;
end;

procedure TwxCustomObjectInspector.PaintCategory(AIndex: Integer);
var
  PItem: PwxPropertyItem;
  R: TRect;
  LDetails: TThemedElementDetails;
  LColor, LTxtColor: TColor;
begin
  CanvasStack.Push(Canvas);

  LDetails := StyleServices.GetElementDetails(tcbCategoryNormal);
  PItem := VisibleItems[AIndex];
  R := ItemRect[AIndex];
  Inc(R.Left, FGutterWidth + 1);
  LColor := FCategoryColor;

  if UseStyleColor then
    LColor := StyleServices.GetStyleColor(scCategoryButtons);

  Canvas.Brush.Color := LColor;
  Canvas.FillRect(R);
  LTxtColor := FCategoryTextColor;

  if UseStyleColor then
    if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) then
      if LColor <> clNone then LTxtColor := LColor;

  Canvas.Font.Color := LTxtColor;
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  Inc(R.Left, FSepTxtDis);
  DrawText(Canvas.Handle, PItem^.Name, - 1, R, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
  Canvas.Font.Style := Canvas.Font.Style - [fsBold];
  CanvasStack.Pop;
end;

procedure TwxCustomObjectInspector.PaintItem(AIndex: Integer);
var
  X, Y, cY: Integer;
  R, pmR: TRect;
  vIndex, POrd: Integer;
  PItem: PwxPropertyItem;
  pOrdPos: Integer;
  DYT, DYB, PrevPos, NextPos: Integer; // delta Y top, delta Y Bottom
  PropName: string;
  PPrevItem, PNextItem: PwxPropertyItem;
  xMax, xMin: Integer;
  HasPlusMinus: Boolean;
  LSaveColor: TColor;
  LColor: TColor;
  HorzDotLeft: Integer;
begin

  if AIndex = FirstItemIndex then begin
    FPropsNeedHint := False;
    FValuesNeedHint := False;
  end;

  PItem := VisibleItems.Items[AIndex];
  vIndex := IndexToVirtualIndex(AIndex);
  HasPlusMinus := False;
  Y := ItemTop(vIndex);
  POrd := ItemOrder(PItem);
  pOrdPos := (POrd * FGutterWidth) + FGutterWidth;
  R := Rect(0, Y, pOrdPos, Y + ItemHeight);

  if AIndex = VisibleItems.Count - 1 then
    R.Height := Height;

  { Background color => will be used to paint property text . }
  LSaveColor := Canvas.Brush.Color;
  LColor := FGutterColor;

  if UseStyleColor then
    LColor := StyleServices.GetSystemColor(clBtnHighlight);

  Canvas.Brush.Color := LColor;
  Canvas.FillRect(R);
  pmR := PlusMinBtnRect[AIndex];

  if PItem^.HasChild and (not CircularLinkedProperties.Contains(PItem^.QualifiedName)) then begin
    DrawExpandbutton(Canvas, pmR.Left, pmR.Top, not PItem.Expanded, pmR.Width);
    HasPlusMinus := True;
  end;

  if not PItem^.IsCategory then begin
    if CanDrawChevron(AIndex) then begin
      cY := CenterPoint(pmR).Y - TwxObjectInspectorButton.ScaledArrowSize;
      X := pOrdPos - (TwxObjectInspectorButton.ScaledArrowSize * 2) - 1; // pOrdPos - (>>)-1
      // cY:=R.Top;
      if HasPlusMinus then
        Dec(X, ValueManager.MinimumPlusSignWidth + 2);

      Canvas.Pen.Color := clWindowText;

      if UseStyleColor then
        Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText);

      DrawChevron(Canvas, sdRight, Point(X, cY), TwxObjectInspectorButton.ScaledArrowSize);
    end;

    if Assigned(OnGetItemFriendlyName) then
      PropName := OnGetItemFriendlyName(Self, PItem)
    else
      PropName := PItem.Name;

    X := pOrdPos + 4;

    if FShowGridLines then begin
      Canvas.Pen.Color := FGridColor;
      DrawHorizontalDottedLine(Canvas, pOrdPos + 1, Y, SplitterPos);
    end;

    if FSelectedIndex = AIndex then begin
      R := Rect(pOrdPos + 1, Y + 1, SplitterPos, Y + ItemHeight);
      LColor := FHighlightColor;

      if UseStyleColor then
        LColor := StyleServices.GetSystemColor(clHighlight);

      Canvas.Brush.Color := LColor;
      Canvas.FillRect(R);
    end else begin
      Canvas.Brush.Color := LSaveColor;
    end;

    R := Rect(X, Y, SplitterPos, Y + ItemHeight);
    LColor := FNameColor;

    // Canvas.Font.Color := clFuchsia;
    if UseStyleColor then
      LColor := StyleServices.GetSystemColor(clBtnText);

    Canvas.Font.Color := LColor;

    if (PItem.Instance is TComponent) and (PItem^.Instance <> PItem^.Component) then
      Canvas.Font.Color := FSubPropertiesColor;

    if IsPropTypeDerivedFromClass(PItem^.AssociatedProperty.PropertyType, TComponent) then
      Canvas.Font.Color := FReferencesColor;

    if (not PItem.AssociatedProperty.IsWritable) and (not PItem^.IsClass) then
      Canvas.Font.Color := FReadOnlyColor;

    Canvas.Refresh;
    R := PropTextRect[AIndex];
    DrawText(Canvas.Handle, PropName, - 1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);

    if Canvas.TextWidth(PropName) > R.Width then
      FPropsNeedHint := True;

    Canvas.Brush.Color := LSaveColor;
    { ====> Paint Item Value <==== }
    PaintItemValue(PItem, AIndex);

    if Canvas.TextWidth(PItem^.ValueAsstring) > ValueTextRect[AIndex].Width then
      FValuesNeedHint := True;

    Canvas.Brush.Color := LSaveColor;
    Canvas.Pen.Color := FGridColor;
    HorzDotLeft := ValueManager.GetExtraRectWidth(PItem);

    if HorzDotLeft > 0 then
      HorzDotLeft := ValueTextRect[AIndex].Left
    else
      HorzDotLeft := SplitterPos;

    if (FSelectedIndex = AIndex) then begin
      DrawHorizontalDottedLine(Canvas, HorzDotLeft, Y, Width);
      DrawHorizontalDottedLine(Canvas, HorzDotLeft, Y + ItemHeight, Width);
    end else begin
      if FShowGridLines then
        DrawHorizontalDottedLine(Canvas, SplitterPos, Y, Width);
    end;
  end;

  {==> Draw gutter line <==}
  if not FShowGutter then
    Exit;

  // Canvas.Pen.Color := clFuchsia;
  LColor := FGutterEdgeColor;

  if UseStyleColor then
    LColor := StyleServices.GetStyleColor(scSplitter);

  Canvas.Pen.Color := LColor;
  Canvas.Refresh;
  DYT := 0;
  DYB := 0;

  if (AIndex - 1) >= 0 then begin
    PPrevItem := VisibleItems.Items[AIndex - 1];
    PrevPos := (ItemOrder(PPrevItem) * FGutterWidth) + FGutterWidth;
    xMax := max(pOrdPos, PrevPos);
    xMin := min(pOrdPos, PrevPos);

    if PrevPos < pOrdPos then begin
      Canvas.MoveTo(xMin, Y - 0);     // Control the edge shape: Y - 0: squre edge
      Canvas.LineTo(xMin + 2, Y);
      Canvas.MoveTo(xMin + 2, Y);
      Canvas.LineTo(xMax - 2, Y);
      Canvas.MoveTo(xMax - 2, Y);
      Canvas.LineTo(xMax, Y + 0);    // Control the edge shape: Y + 0: squre edge
      //DYT := 2;
    end else begin
      if PrevPos > pOrdPos then begin
        Canvas.MoveTo(xMax, Y - 0);  // Control the edge shape: Y - 0: squre edge
        Canvas.LineTo(xMax - 2, Y);
        Canvas.MoveTo(xMax - 2, Y);
        Canvas.LineTo(xMin + 2, Y);
        Canvas.MoveTo(xMin + 2, Y);
        Canvas.LineTo(xMin, Y + 0);  // Control the edge shape: Y + 0: squre edge
        //DYT := 2;
      end;
    end;
  end;

  Canvas.MoveTo(pOrdPos, Y + DYT);

  if (AIndex + 1) < VisibleItems.Count then begin
    PNextItem := VisibleItems.Items[AIndex + 1];
    NextPos := (ItemOrder(PNextItem) * FGutterWidth) + FGutterWidth;
    if pOrdPos <> NextPos then DYB := 0; // DYB = 2 changed to DYB = 0
  end;

  Canvas.LineTo(pOrdPos, Y + ItemHeight - DYB);

  if (AIndex = VisibleItems.Count - 1) then begin
    Canvas.MoveTo(pOrdPos, Y + ItemHeight - DYB);
    Canvas.LineTo(pOrdPos, Height);
  end;
end;

procedure TwxCustomObjectInspector.PaintItemValue(APropItem: PwxPropertyItem; AIndex: Integer);
  procedure doPaintItemValue(ACanvas: TCanvas; AIndex: Integer; const AItem: PwxPropertyItem; ARect: TRect);
  var
    boolVal: Boolean;
    colorRectColor: TColor;
    DC: HDC;
    details: TThemedElementDetails;
    ExtraRect: TRect;
    Inspector: TwxCustomObjectInspector;
    itemValue: TValue;
    QualifiedName: string;
    Style: TCustomStyleServices;
    ValueColor: TColor;
    ValueName: string;
    valueType: PropertyItemValueType;
  begin
    boolVal := False;
    DC := ACanvas.Handle;
    Inspector := Self;
    itemValue := AItem.Value;
    Style := StyleServices;
    valueType := ValueManager.GetValueType(AItem);

    Inspector.CanvasStack.Push(ACanvas);

    if (valueType = vtBool) or (valueType = vtSetElement) then begin
      case valueType of
        vtBool:
          boolVal := TValueConverter.GetValueAs<Boolean>(itemValue);
        vtSetElement:
          boolVal := ElementInSet(GetSetOrdinalValue(itemValue), AItem.SetElementValue);
      end;

      if boolVal then
        details := Style.GetElementDetails(tbCheckBoxCheckedNormal)
      else
        details := Style.GetElementDetails(tbCheckBoxUnCheckedNormal);

      ExtraRect := Inspector.ExtraRect[AIndex];
      Style.DrawElement(DC, details, ExtraRect);
    end else begin
      if valueType = vtColor then begin
        colorRectColor := TValueConverter.GetValueAs<TColor>(itemValue);
        ExtraRect := Inspector.ExtraRect[AIndex];

        if Inspector.SelectedIndex = AIndex then
          InflateRect(ExtraRect, - 1, - 1);

        FillRectWithBorderColor(DC, ExtraRect, ColorToRGB(colorRectColor), clBlack, 1);
      end;
    end;

    Inspector.CanvasStack.Pop;

    QualifiedName := AItem^.QualifiedName;
    ValueName := AItem^.ValueAsstring;

    ValueColor := Inspector.ValueColor;

    if Inspector.UseStyleColor then
      ValueColor := StyleServices.GetSystemColor(clBtnText);

    ACanvas.Font.Color := ValueColor;
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];

    if Inspector.IsValueNotDefault(QualifiedName, ValueName) then begin
      if not Inspector.UseStyleColor then
        ACanvas.Font.Color := Inspector.NonDefaultValueColor;
      if Inspector.BoldNonDefaultValue then
        ACanvas.Font.Style := [fsBold];
    end;

    ARect := Inspector.ValueTextRect[AIndex];
    DrawText(ACanvas.Handle, ValueName, - 1, ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
  end;
var
  rect: TRect;
begin
  CanvasStack.Push(Canvas);
  rect := ValueRect[AIndex];
  doPaintItemValue(Canvas, AIndex, APropItem, rect);
  CanvasStack.Pop;
end;

procedure TwxCustomObjectInspector.RegisterKeys;
begin
  if FAllowSearch and not (csDesigning in ComponentState) then
    FUnRegisterKeys := RegisterHotKey(Handle, 0, 0, VK_TAB)
end;

procedure TwxCustomObjectInspector.SelectItem(AIndex: Integer);
var
  LSI: TScrollInfo;
  procedure DoSetScrollInfo;
  begin
    SetScrollInfo(Handle, SB_VERT, LSI, True);
    if UseStyleBorder then
      InvalidateNC;
  end;

begin
  if (AIndex < 0) then begin
    FSelectedIndex := - 1;
    FSelItem := TwxPropertyItem.Empty;
    Invalidate;
    Exit;
  end;

  if (AIndex < VisibleItems.Count) then begin
    if (AIndex <> FSelectedIndex) then begin

      if Assigned(FOnSelectItem) then
        if not FOnSelectItem(Self, VisibleItems.Items[AIndex]) then
          Exit;

      LSI.cbSize := SizeOf(ScrollInfo);
      LSI.fMask := SIF_POS;
      FSelectedIndex := AIndex;

      if (FSelectedIndex < FirstItemIndex) then begin
        { Index out of page => Need to scroll ! }
        LSI.nPos := FSelectedIndex;
        DoSetScrollInfo;
      end else begin
        if (FSelectedIndex > LastItemIndex - 1) then begin
          { Index out of page => Need to scroll ! }
          LSI.nPos := 1 + FSelectedIndex - MaxItemCount;
          DoSetScrollInfo;
        end;
      end;

      FSelItem := VisibleItems[AIndex]^;
      Invalidate;
      UpdateEditControl;
      Exit;
    end;

    Exit;
  end;

  raise EIndexOutOfRange.Create;
end;

function TwxCustomObjectInspector.SetPropValue(APropItem: PwxPropertyItem; var Value: TValue): Boolean;
begin
  Result := DoSetValue(APropItem, Value);
end;

procedure TwxCustomObjectInspector.SplitterPosChanged(var APos: Integer);
begin
  if (APos < FGutterWidth + 30) then
    APos := FGutterWidth + 30;

  if (APos > ClientWidth - 30) then
    APos := ClientWidth  - 30;

  inherited SplitterPosChanged(APos);
  UpdateEditControl;
end;

procedure TwxCustomObjectInspector.UnRegisterKeys;
begin
  if not FUnRegisterKeys then
    Exit;

  UnregisterHotKey(Handle, 0);
  FUnRegisterKeys := False;
end;

procedure TwxCustomObjectInspector.UpdateEditControl(const AIsSetValue: Boolean);
var
  PItem: PwxPropertyItem;
  BtnWidth: Integer;
  LTxtValRect: TRect;
begin
  if Assigned(FPropInspEdit) and Assigned(FPropInspEdit.Parent) then begin
    FPropInspEdit.PropertyItem := nil;
    FPropInspEdit.Visible := False;
  end;

  if FSelectedIndex < 0 then
    Exit;

  UpdateSelIndex;
  PItem := SelectedItem;

  if Assigned(PItem) then begin
    if PItem^.IsCategory then begin
      if FPropInspEdit.Visible then
        FPropInspEdit.Visible := False;

      Exit;
    end;

    LTxtValRect := ValueTextRect[FSelectedIndex];

    if AIsSetValue and FPropInspEdit.Visible and (Assigned(FPropInspEdit.PropertyItem)) then
      FPropInspEdit.SetValueFromEdit;

    FPropInspEdit.PropertyItem := PItem;
    //with ValueRect[FSelectedIndex] do
    begin
      if ValueManager.HasButton(PItem) then
        BtnWidth := TwxObjectInspectorButton.ScaledWidth // 17
      else
        BtnWidth := 0;

      FPropInspEdit.Left := LTxtValRect.Left;
      FPropInspEdit.Top := LTxtValRect.Top + 3;

      FPropInspEdit.Width := LTxtValRect.Width - BtnWidth;
      FPropInspEdit.Height := ValueRect[FSelectedIndex].Height - 3;
    end;

    FPropInspEdit.Visible := True;
    FPropInspEdit.SetFocus;
    FPropInspEdit.UpdateEditText;
  end;
end;

procedure TwxCustomObjectInspector.UpdateProperties(const Repaint: Boolean);
begin
  UpdateItems;
  UpdateVisibleItems;
  UpdateScrollBar;
  UpdateSelIndex;
  UpdateEditControl(False);

  if Repaint then
    Invalidate;
end;

procedure TwxCustomObjectInspector.UpdateSelIndex;
var
  P: PwxPropertyItem;
begin
  P := SelectedItem;

  if Assigned(P) then begin
    FSelectedIndex := VisibleItems.IndexOf(P);
    SelectItem(FSelectedIndex);
  end;
end;

procedure TwxCustomObjectInspector.WMHotKey(var AMessage: TWMHotKey);
var
  parentForm: TCustomForm;
begin
  inherited;
  parentForm := GetComponentParentForm(Self);

  if Assigned(parentForm) then begin
    if FAllowSearch and (AMessage.HotKey = 0) then begin// HotKey is the id of the Hot Key.
      if Assigned(parentForm.ActiveControl) then begin
        if (ContainsWindow(parentForm.ActiveControl.Handle, Handle)) then begin
          // ActiveControl.Handle is Edit, Handle is Inspector. GetCaretWnd will return the Edit
          // Handle too. So GetCaretWnd = Handle will never be true.
          if GetCaretWnd = Handle then begin // searching
            FSearchText := '';
            UpdateEditControl; // move back to Edit
          end else begin
            // This will move the caret to the inspector property name column; Edit will lost focus
            // leading to Edit.WMKillFocus where Hot Key is unregistered.
            if DoSelectCaret(FSelectedIndex) then
              Exit; // start search
          end;
        end;
      end;
    end;
  end;
end;

procedure TwxCustomObjectInspector.WMKillFocus(var AMessage: TWMKillFocus);
begin
  if GetCaretWnd = Handle then
    DestroyCaret;
end;

procedure TwxCustomObjectInspector.WMLButtonDblClk(var AMessage: TWMLButtonDblClk);
begin
  inherited;
end;

procedure TwxCustomObjectInspector.WMLButtonDown(var AMessage: TWMLButtonDown);
begin
  inherited;
end;

procedure TwxCustomObjectInspector.WMLButtonUp(var AMessage: TWMLButtonUp);
var
  P: TPoint;
begin
  inherited;
  P := Point(AMessage.XPos, AMessage.YPos);

  if (FExtraRectIndex > - 1) and not (ExtraRect[FExtraRectIndex].IsEmpty) then begin
    if ExtraRect[FExtraRectIndex].Contains(P) then
      DoExtraRectClick;
  end;
end;

procedure TwxCustomObjectInspector.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;

  if FAllowSearch then
    CreateCaret(Handle, 0, 1, ItemHeight - 2);
end;

procedure TwxCustomObjectInspector.WndProc(var AMessage: TMessage);
begin
  inherited;
  case AMessage.Msg of
    WM_DESTROY:
      begin
        if not (csDesigning in ComponentState) then
          UnregisterHotKey(Handle, 0);
      end;
    WM_NCLBUTTONDOWN:
      begin
        SendCancelMode(Self); {Make sure that Edit control recieve CM_CANCELMODE message}
      end;
  end;
end;

procedure TwxCustomObjectInspector.Set_AllowSearch(const Value: Boolean);
begin
  if FAllowSearch <> Value then begin
    FAllowSearch := Value;
  end;
end;

procedure TwxCustomObjectInspector.Set_BoldNonDefaultValue(const Value: Boolean);
begin
  if Value <> FBoldNonDefaultValue then begin
    FBoldNonDefaultValue := Value;
    Invalidate;
  end;
end;

function TwxCustomObjectInspector.Get_ExtraRect(Index: Integer): TRect;
var
  w: Integer;
  PItem: PwxPropertyItem;
  R: TRect;
begin
  Result := TRect.Empty;
  PItem := VisibleItems[Index];
  w := ValueManager.GetExtraRectWidth(PItem);

  if w > 0 then begin
    R := ValueRect[Index];
    Result := Rect(0, 0, w, w);
    MoveRectToBoundVCenter(Result, R);
    Result.Width := w;
    Result.Height := w;
    OffsetRect(Result, FSepTxtDis, 0);
  end;
end;

procedure TwxCustomObjectInspector.Set_GridColor(const Value: TColor);
begin
  if FGridColor <> Value then begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_GutterColor(const Value: TColor);
begin
  if Value <> FGutterColor then begin
    FGutterColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_GutterEdgeColor(const Value: TColor);
begin
  if Value <> FGutterEdgeColor then begin
    FGutterEdgeColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_GutterWidth(const Value: Integer);
begin
  if Value > ValueManager.MaximumGutterWidth then
    raise EInvalidGutterWidth.Create;

  if FGutterWidth <> Value then begin
    FGutterWidth := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_HighlightColor(const Value: TColor);
begin
  if Value <> FHighlightColor then begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;

function TwxCustomObjectInspector.Get_ItemRect(Index: Integer): TRect;
var
  vIndex, Y: Integer;
begin
  Result := TRect.Empty;

  if Index > - 1 then begin
    vIndex := IndexToVirtualIndex(Index);
    Y := ItemTop(vIndex);
    Result := Rect(0, Y, Width, Y + ItemHeight);
  end;
end;

procedure TwxCustomObjectInspector.Set_NameColor(const Value: TColor);
begin
  if Value <> FNameColor then begin
    FNameColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_NonDefaultValueColor(const Value: TColor);
begin
  if Value <> FNonDefaultValueColor then begin
    FNonDefaultValueColor := Value;
    Invalidate;
  end;
end;

function TwxCustomObjectInspector.Get_PlusMinBtnRect(Index: Integer): TRect;
var
  X, Y: Integer;
  pOrdPos, POrd: Integer;
  R: TRect;
  cY: Integer;
begin
  Result := TRect.Empty;
  POrd := ItemOrder(VisibleItems.Items[Index]);
  pOrdPos := (POrd * FGutterWidth) + FGutterWidth;
  X := (pOrdPos - ValueManager.MinimumPlusSignWidth) - 3;
  Y := ItemTop(IndexToVirtualIndex(Index));
  R := Rect(0, Y, pOrdPos, Y + ItemHeight);
  cY := CenterPoint(R).Y - (ValueManager.MinimumPlusSignWidth div 2);
  Result := Rect(X, cY, X + ValueManager.MinimumPlusSignWidth, cY + ValueManager.MinimumPlusSignWidth);
end;

function TwxCustomObjectInspector.Get_PropTextRect(Index: Integer): TRect;
begin
  Result := ItemRect[Index];
  Result.Left := (ItemOrder(VisibleItems[Index]) * FGutterWidth) + FGutterWidth + FSepTxtDis;
  Result.Right := SplitterPos;
end;

procedure TwxCustomObjectInspector.Set_ReadOnlyColor(const Value: TColor);
begin
  if FReadOnlyColor <> Value then begin
    FReadOnlyColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_ReferencesColor(const Value: TColor);
begin
  if Value <> FReferencesColor then begin
    FReferencesColor := Value;
    Invalidate;
  end;
end;

function TwxCustomObjectInspector.Get_SelectedItem: PwxPropertyItem;
var
  L: Integer;
begin
  Result := nil;
  L := - 1;

  if (FSelectedIndex > - 1) and (not FSelItem.IsEmpty) then
    L := Items.IndexOfQualifiedName(FSelItem.QualifiedName);

  if L > - 1 then
    Result := Items.Items[L];
end;

procedure TwxCustomObjectInspector.Set_ShowGridLines(const Value: Boolean);
begin
  if Value <> FShowGridLines then begin
    FShowGridLines := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_ShowGutter(const Value: Boolean);
begin
  if Value <> FShowGutter then begin
    FShowGutter := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_SubPropertiesColor(const Value: TColor);
begin
  if Value <> FSubPropertiesColor then begin
    FSubPropertiesColor := Value;
    Invalidate;
  end;
end;

procedure TwxCustomObjectInspector.Set_ValueColor(const Value: TColor);
begin
  if Value <> FValueColor then begin
    FValueColor := Value;
    Invalidate;
  end;
end;

function TwxCustomObjectInspector.Get_ValueRect(Index: Integer): TRect;
var
  vIndex, Y: Integer;
begin
  Result := TRect.Empty;

  if Index > - 1 then begin
    vIndex := IndexToVirtualIndex(Index);
    Y := ItemTop(vIndex);
    Result := Rect(SplitterPos, Y, ClientWidth, Y + ItemHeight);
  end;
end;

function TwxCustomObjectInspector.Get_ValueTextRect(Index: Integer): TRect;
var
  w: Integer;
begin
  w := ExtraRect[Index].Width;
  Result := ValueRect[Index];
  Inc(Result.Left, FSepTxtDis);
  Inc(Result.Left, w);

  if w > 0 then
    Inc(Result.Left, FSepTxtDis);
end;

class constructor TwxObjectInspector.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TwxObjectInspector, TzObjectInspectorScrollingStyleHook);
end;

class destructor TwxObjectInspector.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TwxObjectInspector, TzObjectInspectorScrollingStyleHook);
end;

procedure TwxObjectInspectorDialog.DoCreate;
begin
  inherited;
end;

procedure TwxObjectInspectorDialog.SetPropertyItem(const AItem: PwxPropertyItem);
begin
  if FPropertyItem <> AItem then begin
    FPropertyItem := AItem;
    Setup;
  end;
end;

end.
