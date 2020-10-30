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
  ///   This utility class is used in TzPropertyItem.ValueAsString, which
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
  ///   TzPropertyItem.ValueAsString internally calls FLoatToStrF(ffGeneral,
  ///   TzFloatPreference.ExpPrecision, TzFloatPreference.MaxDigits).
  /// </remarks>
  /// <seealso cref="TzPropertyItem.ValueAsString" />
  /// <seealso cref="SysUtils.TFloatFormat" />
  TzFloatPreference = class(TPersistent)
  private
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
  ///  This class is an extension of TStack. Whenever a Canvas is pushed on stack,
  ///  the associated Pen, Font, and Brush objects are pushed in an internal stack.
  ///  When popping up the Canvas, the saved Pen, Font and Brush objects are restored
  ///  with the Canvas.
  /// </summary>
  TzCanvasStack = class(TStack<TCanvas>)
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
    constructor Create; overload; virtual;
    constructor Create(const ACollection: TEnumerable<TCanvas>); overload;
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

  TzRttiType = class(TRttiType)
    /// <summary>
    ///   Get a list of properties that are declared by the owner object
    ///   itself, or by its parent object and has been assigned a value. If the
    ///   property is declared by parent objects, and has not been assigned a
    ///   value (i.e., not used), it will not be included.
    /// </summary>
    function GetUsedProperties: TArray<TRttiProperty>;
  end;

  /// <summary>
  ///   A pair of Object and its name.
  /// </summary>
  TzObjectNamePair = TPair<TObject, string>;

  /// <summary>
  ///   A container of object name pairs. Key = Object, Value = Name. The life
  ///   cycle of the contained objects are NOT managed by this class.
  /// </summary>
  TzObjectHost = class
  private
    FList: TList<TzObjectNamePair>;
    function GetCount: Integer;
    /// <exception cref="EArgumentOutOfRangeException">
    ///   (AIndex &lt; 0) or (AIndex &gt;= Count)
    /// </exception>
    function GetItem(AIndex: Integer): TzObjectNamePair;
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
    property Item[AIndex: Integer]: TzObjectNamePair read GetItem;
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
  PzPropertyItem = ^TzPropertyItem;
  TzPropertyItemList = class;

  TzPropertyItem = record
  strict private
    FAssociatedProperty: TRttiProperty;
    FCategoryIndex: Integer;
    FCategoryName: string;
    FComponent: TObject;
    FFloatPreference: TzFloatPreference;
    FInstance: TObject;
    FIsCategory: Boolean;
    FObjectVisibility: TMemberVisibility;
    FOwnerList: TzPropertyItemList;
    FParent: PzPropertyItem;
    FQualifiedName: string;
    FSetElementValue: Integer;
    FVisible: Boolean;

    function get_AssociatedComponentParentForm: TCustomForm;
    function get_AssociatedProperty: TRttiProperty;
    procedure set_AssociatedProperty(const AValue: TRttiProperty);
    function get_CategoryIndex: Integer;
    procedure set_CategoryIndex(const AValue: Integer);
    function get_CategoryName: string;
    procedure set_CategoryName(const AValue: string);
    function get_ChildCount: Integer;
    function get_ChildItems(const AIndex: Integer): PzPropertyItem;
    function get_Component: TObject;
    procedure set_Component(const AValue: TObject);
    function get_Expanded: Boolean;
    procedure set_FloatPreference(const AValue: TzFloatPreference);
    function get_HasChild: Boolean;
    function get_Instance: TObject;
    procedure set_Instance(const AValue: TObject);
    function get_IsCategory: Boolean;
    procedure SetIsCategory(const AValue: Boolean);
    function get_IsClass: Boolean;
    function get_IsComponent: Boolean;
    function get_IsEmpty: Boolean;
    function get_IsEnum: Boolean;
    function get_IsSet: Boolean;
    function get_IsSetElement: Boolean;
    function get_MayHaveChild: Boolean;
    function get_Name: string;
    function get_ObjectVisibility: TMemberVisibility;
    procedure set_ObjectVisibility(const AValue: TMemberVisibility);
    function get_Parent: PzPropertyItem;
    procedure set_Parent(const AValue: PzPropertyItem);
    function get_QualifiedName: string;
    procedure set_QualifiedName(const AValue: string);
    function get_SetElementValue: Integer;
    procedure set_SetElementValue(const AValue: Integer);
    function get_Value: TValue;
    function get_ValueAsString: string;
    function get_Visible: Boolean;
    procedure set_Visible(const AValue: Boolean);

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
    class function Empty: TzPropertyItem; static;

    /// <summary>
    ///   Checks whether two items are equal.
    /// </summary>
    /// <param name="AItemToCompare">
    ///   Item to compare.
    /// </param>
    function EqualTo(AItemToCompare: PzPropertyItem): Boolean;

  public
    /// <summary>
    ///   If the component being inspected is a TComponent, this provides its
    ///   parent form. This is useful for getting event handlers defined as
    ///   members of the parent form.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property AssociatedComponentParentForm: TCustomForm read get_AssociatedComponentParentForm;

    /// <summary>
    ///   This is the associated RTTI property of the item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property AssociatedProperty: TRttiProperty read get_AssociatedProperty write set_AssociatedProperty;

    /// <summary>
    ///   If the current item is a category item, this provides the index of
    ///   the subject category in the category list maintained by the
    ///   inspector.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property CategoryIndex: Integer read get_CategoryIndex write set_CategoryIndex;

    /// <summary>
    ///   If the current item is a category item, this provides the name of the
    ///   subject category.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property CategoryName: string read get_CategoryName write set_CategoryName;

    /// <summary>
    ///   Get the total count of child items.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property ChildCount: Integer read get_ChildCount;

    /// <summary>
    ///   Get the child item by index. If the index is out of bound, a nil will
    ///   be returned.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property ChildItems[const AIndex: Integer]: PzPropertyItem read get_ChildItems;

    /// <summary>
    ///   The component being inspected. It can be a TObject, or a TComponent.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Component: TObject read get_Component write set_Component;

    /// <summary>
    ///   The subject item has child items, and is expanded.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property Expanded: Boolean read get_Expanded;

    /// <summary>
    ///   Floating point preference for presenting floating point numbers. It
    ///   is taken from the inspector. The life cycle is managed by the owning
    ///   inspector, not the subject item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property FloatPreference: TzFloatPreference write set_FloatPreference;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property HasChild: Boolean read get_HasChild;

    /// <summary>
    ///   The instance of the property variable associated with this item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Instance: TObject read get_Instance write set_Instance;

    /// <summary>
    ///   Whether this item is a category.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property IsCategory: Boolean read get_IsCategory write SetIsCategory;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsClass: Boolean read get_IsClass;

    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsComponent: Boolean read get_IsComponent;

    /// <summary>
    ///   Check current item is empty or not. Empty means all members are
    ///   zeroed-out.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsEmpty: Boolean read get_IsEmpty;

    /// <summary>
    ///   Check if current item is enumeration type.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsEnum: Boolean read get_IsEnum;

    /// <summary>
    ///   Check if current item is a set.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsSet: Boolean read get_IsSet;

    /// <summary>
    ///   Check if current element is a set element.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property IsSetElement: Boolean read get_IsSetElement;

    /// <summary>
    ///   Whether the item may have child. Class, Set and Category can have
    ///   children.
    /// </summary>
    /// <remarks>
    ///   Read-Only.
    /// </remarks>
    property MayHaveChild: Boolean read get_MayHaveChild;

    /// <summary>
    ///   The name of the property associated with the item.
    /// </summary>
    /// <remarks>
    ///   Read Only.
    /// </remarks>
    property Name: string read get_Name;

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
    property ObjectVisibility: TMemberVisibility read get_ObjectVisibility write set_ObjectVisibility;

    /// <summary>
    ///   OwnerList of the item, i.e., the master item list.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property OwnerList: TzPropertyItemList read FOwnerList write FOwnerList;

    /// <summary>
    ///   Parent item of the current item.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Parent: PzPropertyItem read get_Parent write set_Parent;

    /// <summary>
    ///   Qualified name of the item. This may be prefixed with a category name, or suffixed with
    ///   an index number, seperated by a dot.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property QualifiedName: string read get_QualifiedName write set_QualifiedName;

    /// <summary>
    ///   If the subject item is an element of a set, SetElementValue reflects
    ///   the associated ordinal value of the element.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property SetElementValue: Integer read get_SetElementValue write set_SetElementValue;

    /// <summary>
    ///   Get the value of this item, based on the RTTI type, and the
    ///   associated instance.
    /// </summary>
    /// <remarks>
    ///   Read Only.
    /// </remarks>
    property Value: TValue read get_Value;

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
    property ValueAsString: string read get_ValueAsString;

    /// <summary>
    ///   Whether the item is visible.
    /// </summary>
    /// <remarks>
    ///   Read-Write. Externally set after creation.
    /// </remarks>
    property Visible: Boolean read get_Visible write set_Visible;
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
  TzRecordList<T, PT> = class(TObject)
  private
    type
      /// <summary>
      ///   A type definition for the pointer type of T. It is needed so we can
      ///   play all the trick to allow the generics capability of
      ///   TzRecordList&lt;T,PT&gt;.
      /// </summary>
      PtrT = ^T;
  strict private
    /// <summary>
    ///   Internal list of record pointers. The generic type PT is converted to general pointer
    ///   before saving to the list.
    /// </summary>
    FRecPtrList: TList;
    function get_Item(const AIndex: Integer): PT;
    function get_Count: Integer;
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
    property Items[const AIndex: Integer]: PT read get_Item;

    /// <summary>
    ///   Total number of items in the list.
    /// </summary>
    property Count: Integer read get_Count;
  end;

  TzPropertyItemList = class(TzRecordList<TzPropertyItem, PzPropertyItem>)
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
    AItem: PzPropertyItem
  ): Boolean of object;

  TzPropertyItemGetFriendlyNameEvent = function(
    Sender: TControl;
    AItem: PzPropertyItem
  ): string of object;

  TzPropertyItemSetValueEvent = function(
    Sender: TControl;
    AItem: PzPropertyItem;
    var ANewValue: TValue
  ): Boolean of object;

  TzSplitterPosChangedEvent = procedure(
    Sender: TControl;
    var APos: Integer
  ) of object;

type
  TzCustomControl = class(TCustomControl)
  strict private
    FIsMouseLButtonDown: Boolean;
    {$REGION 'Property gettors and settors'}
    function get_IsMouseDown: Boolean;
    function get_IsMouseInControl: Boolean;
    function get_IsVclStyleUsed: Boolean;
    function get_UseStyleBorder: Boolean;
    function get_UseStyleColor: Boolean;
    function get_UseStyleFont: Boolean;
    {$ENDREGION}
  private
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  public
    property IsMouseDown: Boolean read get_IsMouseDown;
    property IsMouseInControl: Boolean read get_IsMouseInControl;
    property IsVclStyleUsed: Boolean read get_IsVclStyleUsed;
    property UseStyleBorder: Boolean read get_UseStyleBorder;
    property UseStyleColor: Boolean read get_UseStyleColor;
    property UseStyleFont: Boolean read get_UseStyleFont;
  end;

  TzObjectInspectorBase = class abstract(TzCustomControl)
  strict private
    FCanvasStack: TzCanvasStack;
    FCategories: TList<string>;
    FCircularLinkedProperties: TList<string>;
    FComponent: TObject;
    FComponentClassType: TClass;
    FContext: TRttiContext;
    FDefaultCategoryName: string;
    FDefaultPropertyValueMap: TDictionary<string, string>;
    FExpandedList: TList<string>;
    FFloatPreference: TzFloatPreference;
    FIsSettingComponent: Boolean;
    FItems: TzPropertyItemList;
    FLockUpdate: Boolean;
    FObjectVisibility: TMemberVisibility;
    FOnAutoExpandItemOnInit: TzPropertyItemEvent;
    FOnBeforeAddItem: TzPropertyItemEvent;
    FPropertyCategoryMap: TDictionary<string, Integer>;
    FPropertyInstances: TDictionary<string, TObject>;
    FRttiType: TRttiType;
    FSaveVisibleItems: TList<string>;
    FSortByCategory: Boolean;
    FVisibleItems: TList<PzPropertyItem>;
    {$REGION 'Property gettors and settors'}
    procedure set_Component(AValue: TObject);
    function get_FloatPreference: TzFloatPreference;
    procedure set_ObjectVisibility(const AValue: TMemberVisibility);
    procedure set_SortByCategory(const AValue: Boolean);
    {$ENDREGION}
  strict protected
    function CircularLinkedProperties: TList<string>;
    function ExpandedList: TList<string>;
    function ItemOrder(PItem: PzPropertyItem): Integer;
    function LockUpdate: Boolean;
    function SaveVisibleItems: TList<string>;
    function VisibleItems: TList<PzPropertyItem>;
  protected
    procedure Changed; virtual;
    procedure ComponentChanged; virtual;
    procedure UpdateItems;
    procedure UpdateVisibleItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure ClearRegisteredCategorys;
    procedure EndUpdate;
    procedure Invalidate; override;

    function IsItemCircularLink(AItem: PzPropertyItem): Boolean;
    function IsValueNotDefault(AQualifiedName: string; AValue: string): Boolean;
    function ItemNeedUpdate(AItem: PzPropertyItem): Boolean;
    function NeedUpdate: Boolean;

    procedure RegisterPropertyInCategory(const ACategoryName, APropertyName: string);
    procedure UpdateProperties(const ADoRepaint: Boolean = False); virtual; abstract;

    property CanvasStack: TzCanvasStack read FCanvasStack;
    property Categories: TList<string> read FCategories;
    property Component: TObject read FComponent write set_Component;
    property ComponentClassType: TClass read FComponentClassType;
    property DefaultCategoryName: string read FDefaultCategoryName write FDefaultCategoryName;
    property DefaultPropertyValue: TDictionary<string, string> read FDefaultPropertyValueMap;
    property FloatPreference: TzFloatPreference read get_FloatPreference;
    property Items: TzPropertyItemList read FItems;
    property ObjectVisibility: TMemberVisibility read FObjectVisibility write set_ObjectVisibility default mvPublic;
    property SortByCategory: Boolean read FSortByCategory write set_SortByCategory;
    property OnAutoExpandItemOnInit: TzPropertyItemEvent read FOnAutoExpandItemOnInit write FOnAutoExpandItemOnInit;
    property OnBeforeAddItem: TzPropertyItemEvent read FOnBeforeAddItem write FOnBeforeAddItem;
  end;

  /// <summary>
  ///   Provides BorderStyle, ItemHeight and ReadOnly properties.
  /// </summary>
  TzObjectInspectorList = class abstract(TzObjectInspectorBase)
  strict private
    FBorderStyle: TBorderStyle;
    FItemHeight: Integer;
    FReadOnly: Boolean;
    {$REGION 'Property gettors and settors'}
    procedure set_BorderStyle(const Value: TBorderStyle);
    {$ENDREGION}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BorderStyle: TBorderStyle read FBorderStyle write set_BorderStyle;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

  TzObjectInspectorSplitterList = class abstract(TzObjectInspectorList)
  strict private
    FFixedSplitter: Boolean;
    FOnSplitterPosChanged: TzSplitterPosChangedEvent;
    FSplitterColor: TColor;
    FSplitterDown: Boolean;
    FSplitterPos: Integer;
    {$REGION 'Property gettors and settors'}
    procedure set_SplitterColor(const AValue: TColor);
    procedure set_SplitterPos(const AValue: Integer);
    function get_SplitterRect: TRect;
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
    property SplitterColor: TColor read FSplitterColor write set_SplitterColor;
    property SplitterPos: Integer read FSplitterPos write set_SplitterPos;
    property SplitterRect: TRect read get_SplitterRect;
    property OnSplitterPosChanged: TzSplitterPosChangedEvent read FOnSplitterPosChanged write FOnSplitterPosChanged;
  end;

  TzObjectInspectorHeaderList = class abstract(TzObjectInspectorSplitterList)
  strict private
    FHeaderPressed: Boolean;
    FHeaderPropPressed: Boolean;
    FHeaderPropText: string;
    FHeaderValuePressed: Boolean;
    FHeaderValueText: string;
    FOnHeaderMouseDown: TzHeaderMouseDownEvent;
    FShowHeader: Boolean;
    {$REGION 'Property gettors and settors'}
    function get_HeaderPropRect: TRect;
    procedure set_HeaderPropText(const AValue: string);
    function get_HeaderRect: TRect;
    function get_HeaderValueRect: TRect;
    procedure set_HeaderValueText(const AValue: string);
    procedure set_ShowHeader(const AValue: Boolean);
    {$ENDREGION}
  private
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure Paint; override;
    procedure PaintHeader; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property HeaderPropRect: TRect read get_HeaderPropRect;
    property HeaderPropText: string read FHeaderPropText write set_HeaderPropText;
    property HeaderRect: TRect read get_HeaderRect;
    property HeaderValueRect: TRect read get_HeaderValueRect;
    property HeaderValueText: string read FHeaderValueText write set_HeaderValueText;
    property ShowHeader: Boolean read FShowHeader write set_ShowHeader;
    property OnHeaderMouseDown: TzHeaderMouseDownEvent read FOnHeaderMouseDown write FOnHeaderMouseDown;
  end;

  TzObjectInspectorScrollList = class abstract(TzObjectInspectorHeaderList)
  strict private
    FPrevScrollPos: Integer;
    FScrollInfo: TScrollInfo;
    {$REGION 'Property gettors and settors'}
    function get_VisiblePropCount: Integer;
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
    property VisiblePropCount: Integer read get_VisiblePropCount;
  end;

{$REGION 'Edit Controls'}
type
  TzObjectInspectorButton = class(TzCustomControl)
  const
    DefaultArrowSize = 3;
    DefaultWidth = 17;
  private
    FDropDown: Boolean;
  class var
    FScaledArrowSize: Integer;
    FScaledWidth: Integer;
    class constructor Create;
    class procedure SetScaledArrowSize(const AValue: Integer); static;
    class procedure SetScaledWidth(const AValue: Integer); static;
  protected
    procedure Paint; override;
    procedure WMLButtonDown(var AMessage: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var AMessage: TWMLButtonUp); message WM_LBUTTONUP;
  public
    constructor Create(AOwner: TComponent); override;
    property DropDown: Boolean read FDropDown write FDropDown;
    property OnMouseDown;
    class property ScaledArrowSize: Integer read FScaledArrowSize write SetScaledArrowSize;
    class property ScaledWidth: Integer read FScaledWidth write SetScaledWidth;
  end;

  TzObjectInspectorListBox = class(TCustomListBox)
  private
    FItem: Pointer;
    FItemEdit: TCustomEdit;
    procedure SetItem(const AValue: Pointer);
    procedure SetItemEdit(const AValue: TCustomEdit);
  protected
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
  public
    property Item: Pointer read FItem write SetItem;
    property ItemEdit: TCustomEdit read FItemEdit write SetItemEdit;
    property ItemHeight;
    property OnMouseDown;
  end;

  TzObjectInspectorListBoxClass = class of TzObjectInspectorListBox;

  { =>This class is used to test if the ListBox has custom items.
    =>Any CustomListBox must be derived from this class ! }
  TzObjectInspectorCustomListBox = class abstract(TzObjectInspectorListBox)
  protected
    procedure CreateWnd; override;
    procedure PopulateList; virtual; abstract;
  end;

  /// <summary>
  ///   Provides a list box filled with color items, each item has a sample
  ///   rectangle showing the color and the name of the color.
  /// </summary>
  TzColorListBox = class(TzObjectInspectorCustomListBox)
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
  TzCursorListBox = class(TzObjectInspectorCustomListBox)
  private
    procedure CursorCallBack(const AName: string);
    function GetCursor(AIndex: Integer): TCursor;
  protected
    procedure DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState); override;
    procedure PopulateList; override; final;
  public
    constructor Create(AOwner: TComponent); override;
    property Cursors[AIndex: Integer]: TCursor read GetCursor;
  end;

  /// <summary>
  ///   Provides a list box with a list of predefined short cuts.
  /// </summary>
  TzShortCutListBox = class(TzObjectInspectorCustomListBox)
  private
    procedure EnumShortCuts;
    function GetShortCut(AIndex: Integer): TShortCut;
  protected
    procedure PopulateList; override; final;
  public
    constructor Create(AOwner: TComponent); override;
    property ShortCuts[AIndex: Integer]: TShortCut read GetShortCut;
  end;

  TzObjectInspectorEdit = class(TCustomEdit)
  strict private
    FButton: TzObjectInspectorButton;
    FDefaultSelectedIndex: Integer;
    FOwnerInspector: TzCustomControl;
    FListBox: TzObjectInspectorListBox;
    FPropertyItem: PzPropertyItem;
    FTextChanged: Boolean;
    {$REGION 'Gettors and Settors'}
    function GetListBox: TzObjectInspectorListBox;
    function GetPropertyItem: PzPropertyItem;
    procedure SetPropertyItem(const AItem: PzPropertyItem);
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
    constructor Create(AOwnerInspector: TzCustomControl); reintroduce;
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
    property ListBox: TzObjectInspectorListBox read GetListBox;
    property PropertyItem: PzPropertyItem read GetPropertyItem write SetPropertyItem;
  end;
{$ENDREGION}

  TzCustomObjectInspector = class(TzObjectInspectorScrollList)
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
    FPropInspEdit: TzObjectInspectorEdit;
    FPropsNeedHint: Boolean;
    FReadOnlyColor: TColor;
    FReferencesColor: TColor;
    FSearchText: string;
    FSelectedIndex: Integer;
    FSelItem: TzPropertyItem;
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
    procedure set_AllowSearch(const Value: Boolean);
    procedure set_BoldNonDefaultValue(const Value: Boolean);
    function get_ExtraRect(Index: Integer): TRect;
    procedure set_GridColor(const Value: TColor);
    procedure set_GutterColor(const Value: TColor);
    procedure set_GutterEdgeColor(const Value: TColor);
    procedure set_GutterWidth(const Value: Integer);
    procedure set_HighlightColor(const Value: TColor);
    function get_ItemRect(Index: Integer): TRect;
    procedure set_NameColor(const Value: TColor);
    procedure set_NonDefaultValueColor(const Value: TColor);
    function get_PlusMinBtnRect(Index: Integer): TRect;
    function get_PropTextRect(Index: Integer): TRect;
    procedure set_ReadOnlyColor(const Value: TColor);
    procedure set_ReferencesColor(const Value: TColor);
    function GetSelectedItem: PzPropertyItem;
    procedure set_ShowGridLines(const Value: Boolean);
    procedure set_ShowGutter(const Value: Boolean);
    procedure set_SubPropertiesColor(const Value: TColor);
    procedure set_ValueColor(const Value: TColor);
    function get_ValueRect(Index: Integer): TRect;
    function get_ValueTextRect(Index: Integer): TRect;
    {$ENDREGION}
  private
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMSTYLECHANGED(var Message: TMessage); message CM_STYLECHANGED;
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure WMKILLFOCUS(var Msg: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure WMLBUTTONDBLCLK(var Message: TWMLBUTTONDBLCLK); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSETFOCUS(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    function CanDrawChevron(Index: Integer): Boolean;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateWnd; override;
    function DoCollapseItem(PItem: PzPropertyItem): Boolean;
    function DoExpandItem(PItem: PzPropertyItem): Boolean;
    procedure DoExtraRectClick;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoSelectCaret(Index: Integer): Boolean;
    function DoSetValue(PropItem: PzPropertyItem; var Value: TValue): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintCategory(Index: Integer); virtual;
    procedure PaintItem(Index: Integer); override;
    procedure PaintItemValue(PItem: PzPropertyItem; Index: Integer); virtual;
    procedure SplitterPosChanged(var Pos: Integer); override;
    procedure UpdateEditControl(const SetValue: Boolean = True);
    procedure UpdateSelIndex;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CollapseAll;
    function CollapseItem(PItem: PzPropertyItem): Boolean;
    procedure ExpandAll;
    function ExpandItem(PItem: PzPropertyItem): Boolean;
    procedure RegisterKeys;
    procedure SelectItem(Index: Integer);
    function SetPropValue(PropItem: PzPropertyItem; var Value: TValue): Boolean;
    procedure UnRegisterKeys;
    procedure UpdateProperties(const Repaint: Boolean = False); override;

    property AllowSearch: Boolean read FAllowSearch write set_AllowSearch;
    property AutoCompleteText: Boolean read FAutoCompleteText write FAutoCompleteText;
    property BoldNonDefaultValue: Boolean read FBoldNonDefaultValue write set_BoldNonDefaultValue;
    property ClickTime: Integer read FClickTime write FClickTime;
    property ExtraRect[Index: Integer]: TRect read get_ExtraRect;
    property GridColor: TColor read FGridColor write set_GridColor;
    property GutterColor: TColor read FGutterColor write set_GutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write set_GutterEdgeColor;
    property GutterWidth: Integer read FGutterWidth write set_GutterWidth;
    property HighlightColor: TColor read FHighlightColor write set_HighlightColor;
    property ItemRect[Index: Integer]: TRect read get_ItemRect;
    property NameColor: TColor read FNameColor write set_NameColor;
    property NonDefaultValueColor: TColor read FNonDefaultValueColor write set_NonDefaultValueColor;
    property PlusMinBtnRect[Index: Integer]: TRect read get_PlusMinBtnRect;
    property PropTextRect[Index: Integer]: TRect read get_PropTextRect;
    property ReadOnlyColor: TColor read FReadOnlyColor write set_ReadOnlyColor;
    property ReferencesColor: TColor read FReferencesColor write set_ReferencesColor;
    property SelectedIndex: Integer read FSelectedIndex;
    property SelectedItem: PzPropertyItem read GetSelectedItem;
    property ShowGridLines: Boolean read FShowGridLines write set_ShowGridLines;
    property ShowGutter: Boolean read FShowGutter write set_ShowGutter;
    property ShowItemHint: Boolean read FShowItemHint write FShowItemHint;
    property SubPropertiesColor: TColor read FSubPropertiesColor write set_SubPropertiesColor;
    property TrackChange: Boolean read FTrackChange write FTrackChange;
    property ValueColor: TColor read FValueColor write set_ValueColor;
    property ValueRect[Index: Integer]: TRect read get_ValueRect;
    property ValueTextRect[Index: Integer]: TRect read get_ValueTextRect;
    property OnCollapseItem: TzPropertyItemEvent read FOnCollapseItem write FOnCollapseItem;
    property OnExpandItem: TzPropertyItemEvent read FOnExpandItem write FOnExpandItem;
    property OnGetItemFriendlyName: TzPropertyItemGetFriendlyNameEvent read FOnGetItemFriendlyName write FOnGetItemFriendlyName;
    property OnGetItemReadOnly: TzPropertyItemEvent read FOnGetItemReadOnly write FOnGetItemReadOnly;
    property OnItemSetValue: TzPropertyItemSetValueEvent read FOnItemSetValue write FOnItemSetValue;
    property OnSelectItem: TzPropertyItemEvent read FOnSelectItem write FOnSelectItem;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TzObjectInspector = class(TzCustomObjectInspector)
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
  TzObjectInspectorDialogBase = class abstract(TForm)
  private
    FPropertyItem: PzPropertyItem;
    procedure SetPropertyItem(const AItem: PzPropertyItem);
  protected
    procedure DoCreate; override;
    procedure Setup; virtual; abstract;
  public
    property PropertyItem: PzPropertyItem read FPropertyItem write SetPropertyItem;
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
procedure TzObjectInspectorListBox.CreateParams(var AParams: TCreateParams);
begin
  inherited;
  AParams.Style := AParams.Style or WS_BORDER;
  AParams.ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  AParams.WindowClass.Style := CS_SAVEBITS;
end;

procedure TzObjectInspectorListBox.CreateWnd;
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

procedure TzObjectInspectorListBox.MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Self.HandleAllocated then
    ShowWindow(Self.Handle, SW_HIDE);
end;

procedure TzObjectInspectorListBox.SetItem(const AValue: Pointer);
begin
  FItem := AValue;
end;

procedure TzObjectInspectorListBox.SetItemEdit(const AValue: TCustomEdit);
begin
  FItemEdit := AValue;
end;

procedure TzObjectInspectorCustomListBox.CreateWnd;
begin
  inherited;
  PopulateList;
end;

// The list box is owner-drawn, but each item in the list box is the height specified by
// the ItemHeight property. Each time an item is displayed in an lbOwnerDrawFixed list box,
// the OnDrawItem event occurs. The event handler for OnDrawItem draws the specified item.
// The ItemHeight property determines the height of each of the items.
constructor TzColorListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawFixed;
end;

procedure TzColorListBox.ColorCallBack(const AName: string);
var
  color: TColor;
begin
  color := StringToColor(AName);
  Items.AddObject(AName, TObject(color));
end;

procedure TzColorListBox.DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState);
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

function TzColorListBox.GetColor(AIndex: Integer): TColor;
begin
  Result := TColor(Items.Objects[AIndex]);
end;

procedure TzColorListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  GetColorValues(ColorCallBack);
  Items.EndUpdate;
end;

constructor TzCursorListBox.Create(AOwner: TComponent);
const
  cDefaultItemHeight = 35;
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawFixed;
  ItemHeight := cDefaultItemHeight;
end;

procedure TzCursorListBox.CursorCallBack(const AName: string);
var
  cursor: TCursor;
begin
  cursor := StringToCursor(AName);
  Items.AddObject(AName, TObject(Cursor));
end;

procedure TzCursorListBox.DrawItem(AIndex: Integer; ARect: TRect; AOwnerDrawState: TOwnerDrawState);
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

function TzCursorListBox.GetCursor(AIndex: Integer): TCursor;
begin
  Result := TCursor(Items.Objects[AIndex]);
end;

procedure TzCursorListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  GetCursorValues(CursorCallBack);
  Items.EndUpdate;
end;

constructor TzShortCutListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TzShortCutListBox.EnumShortCuts;
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

function TzShortCutListBox.GetShortCut(AIndex: Integer): TShortCut;
begin
  Result := TShortCut(Items.Objects[AIndex]);
end;

procedure TzShortCutListBox.PopulateList;
begin
  Items.Clear;
  Items.BeginUpdate;
  EnumShortCuts;
  Items.EndUpdate;
end;

class constructor TzObjectInspectorButton.Create;
begin
  FScaledWidth := DefaultWidth;
  FScaledArrowSize := DefaultArrowSize;
end;

constructor TzObjectInspectorButton.Create(AOwner: TComponent);
begin
  inherited;
  FDropDown := True;
end;

procedure TzObjectInspectorButton.Paint;
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
  end
  else begin
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

class procedure TzObjectInspectorButton.SetScaledArrowSize(const AValue: Integer);
begin
  FScaledArrowSize := AValue;
end;

class procedure TzObjectInspectorButton.SetScaledWidth(const AValue: Integer);
begin
  FScaledWidth := AValue;
end;

procedure TzObjectInspectorButton.WMLButtonDown(var AMessage: TWMLButtonDown);
begin
  inherited;
  Invalidate; // So as to Paint
end;

procedure TzObjectInspectorButton.WMLButtonUp(var AMessage: TWMLButtonUp);
begin
  inherited;
  Invalidate; // So as to Paint
end;

constructor TzObjectInspectorEdit.Create(AOwnerInspector: TzCustomControl);
begin
  inherited Create(AOwnerInspector);
  ParentCtl3D := False;
  BorderStyle := TBorderStyle(0);
  Ctl3D := False;
  TabStop := False;
  FListBox := nil;
  FPropertyItem := nil;
  FOwnerInspector := AOwnerInspector;
  FButton := TzObjectInspectorButton.Create(Self);
  FButton.OnMouseDown := ButtonClick;
end;

procedure TzObjectInspectorEdit.ButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // When Button is visible, HasButton must be True, meaning either ListBox, or Dialog
  if Assigned(FListBox) then begin
    if IsWindowVisible(FListBox.Handle) then
      HideList
    else
      ShowList;
  end
  else begin
    ShowModalDialog;
  end;
end;

procedure TzObjectInspectorEdit.CMCancelMode(var AMessage: TCMCancelMode);
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

procedure TzObjectInspectorEdit.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  FButton.Visible := Self.Visible;
  UpdateButton;
end;

procedure TzObjectInspectorEdit.DoDblClick;
var
  listIndex: Integer;
begin
  inherited;

  if ValueManager.HasDialog(FPropertyItem) then begin
    ShowModalDialog
  end
  else begin
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

procedure TzObjectInspectorEdit.DoSetValueFromEdit;
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
    TzCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, value);
    Exit;
  end
  else begin
    if Assigned(FListBox) then begin
      listIndex := FListBox.Items.IndexOf(str);

      if listIndex > - 1 then begin
        FListBox.Selected[listIndex] := True;
        DoSetValueFromList;
        Exit;
      end
      else begin
        if not ValueManager.ValueHasOpenProbabilities(FPropertyItem) then begin
          raise EInvalidPropertyValue.Create(FPropertyItem.Name, str);
          Exit; // Is this exit necessary?
        end;
      end
    end;
  end;

  value := ValueManager.StrToValue(FPropertyItem, str);
  TzCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, value);
  SelectAll;
end;

procedure TzObjectInspectorEdit.DoSetValueFromList;
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
  end
  else begin
    newValue := ValueManager.GetValue(FPropertyItem, obj);
  end;

  TzCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, newValue);
  // Update the log of the selected index.
  FDefaultSelectedIndex := FListBox.ItemIndex;
end;

function TzObjectInspectorEdit.GetListBox: TzObjectInspectorListBox;
begin
  Result := FListBox;
end;

function TzObjectInspectorEdit.GetPropertyItem: PzPropertyItem;
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
procedure TzObjectInspectorEdit.HideList;
begin
  if Assigned(FListBox) and (FListBox.HandleAllocated) then
    ShowWindow(FListBox.Handle, SW_HIDE);
end;

procedure TzObjectInspectorEdit.InitList;
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
  if not (FListBox is TzObjectInspectorCustomListBox) then
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

procedure TzObjectInspectorEdit.ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoSetValueFromList;
end;

procedure TzObjectInspectorEdit.PropertyItemChanged;
var
  inspector: TzCustomObjectInspector;
begin
  // If the newly changed value is nil (which is possible), then exit.
  if not Assigned(FPropertyItem) then
    Exit;

  // Reset TextChanged flag to False.
  FTextChanged := False;
  // Type cast from TzCustomControl to TzCustomObjectInspector.
  inspector := TzCustomObjectInspector(FOwnerInspector);

  // Determine ReadOnly
  if inspector.ReadOnly then begin
    Self.ReadOnly := True;
  end
  else begin
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

procedure TzObjectInspectorEdit.SetPropertyItem(const AItem: PzPropertyItem);
begin
  if not FPropertyItem.EqualTo(AItem) then begin
    FPropertyItem := AItem;
    PropertyItemChanged;
  end;
end;

procedure TzObjectInspectorEdit.SetValueFromEdit;
begin
  DoSetValueFromEdit;
end;

procedure TzObjectInspectorEdit.ShowList;
const
  cListBoxBottomPositionMargin = 50;
  cListBoxHeightMargin = 4;
  cListBoxTopPositionMargin = 2;
var
  inspector: TzCustomObjectInspector;
  listBoxPos: TPoint;
  newListBoxHeight: Integer;
begin
  if (not Assigned(FListBox)) or (FListBox.Items.Count = 0) then
    Exit;

  inspector := TzCustomObjectInspector(FOwnerInspector);

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

procedure TzObjectInspectorEdit.ShowModalDialog;
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

    if (not (dlg is TCommonDialog)) and (not (dlg is TzObjectInspectorDialogBase)) then
      raise EInvalidDialogClass.Create;

    ValueManager.PerformDialogAction(FPropertyItem, dlg, dcInit);
    mr := ValueManager.PerformDialogAction(FPropertyItem, dlg, dcShow);
    PostMessage(Handle, WM_LBUTTONUP, 0, 0);

    if mr = mrOk then begin
      ValueManager.PerformDialogAction(FPropertyItem, dlg, dcFinished);
      dlgResult := ValueManager.DialogResultValue(FPropertyItem, dlg);
      TzCustomObjectInspector(FOwnerInspector).SetPropValue(FPropertyItem, dlgResult);
    end;

    ValueManager.PerformDialogAction(FPropertyItem, dlg, dcBeforeDestroy);
    FreeAndNil(dlg);
  end;
end;

procedure TzObjectInspectorEdit.UpdateButton;
const
  cButtonTopPositionMargin = 3;
var
  inspector: TzCustomObjectInspector;
begin
  FButton.Visible := False; // Reset to invisible.
  inspector := TzCustomObjectInspector(FOwnerInspector);

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
  FButton.Left := Self.Parent.ClientWidth - TzObjectInspectorButton.ScaledWidth;
  FButton.Top := Top - cButtonTopPositionMargin;
  FButton.Height := inspector.ItemHeight;
  FButton.Width := TzObjectInspectorButton.ScaledWidth;
  FButton.Visible := True;
end;

procedure TzObjectInspectorEdit.UpdateEditText;
var
  inspector: TzCustomObjectInspector;
begin
  inspector := TzCustomObjectInspector(FOwnerInspector);
  Self.Text := FPropertyItem.ValueAsString;

  if not inspector.DefaultPropertyValue.ContainsKey(FPropertyItem.QualifiedName) then
    inspector.DefaultPropertyValue.Add(FPropertyItem.QualifiedName, Self.Text);

  SelectAll;
end;

procedure TzObjectInspectorEdit.WMChar(var AMessage: TWMChar);
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

  if not TzCustomObjectInspector(FOwnerInspector).AutoCompleteText then
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

procedure TzObjectInspectorEdit.WMKeyDown(var AMessage: TWMKeyDown);
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
    selectedIndex := TzCustomObjectInspector(FOwnerInspector).SelectedIndex;

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
        selectedIndex := Min(TzCustomObjectInspector(FOwnerInspector).VisiblePropCount - 1, selectedIndex);
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
    TzCustomObjectInspector(FOwnerInspector).SelectItem(selectedIndex);
end;

procedure TzObjectInspectorEdit.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;

  if Assigned(FListBox) then begin
    if IsWindowVisible(FListBox.Handle) then
      HideList;
  end;

  DoSetValueFromEdit;
  TzCustomObjectInspector(FOwnerInspector).UnRegisterKeys;
end;

procedure TzObjectInspectorEdit.WMLButtonDblClk(var AMessage: TWMLButtonDblClk);
begin
  DoDblClick;
end;

// Useless function for WMLButtonDown?
procedure TzObjectInspectorEdit.WMLButtonDown(var AMessage: TWMLButtonDown);
const
  cMinDoubleClickGap = 200;
var
  curClickTime, elapsedTime: Integer;
begin
  { When PropInspEdit is activated, the Inspector will not fire the WMLBUTTONDBLCLK message .
    => we need to detect the double click manually! }
  if TzCustomObjectInspector(FOwnerInspector).ClickTime <> - 1 then begin
    curClickTime := GetTickCount;
    elapsedTime := curClickTime - TzCustomObjectInspector(FOwnerInspector).ClickTime;
 
    if (elapsedTime) < cMinDoubleClickGap then
      DoDblClick;

    TzCustomObjectInspector(FOwnerInspector).ClickTime := - 1;
  end;
  inherited;
end;

// Every focus we need to register key.  This means, every time we click the Edit's window
// a HotKey registration will occur. Then, we press the registered hot key (TAB), the caret will
// be moved to the property name column, causing the Edit losing focus hence unregister the hot
// key. After the caret is moved to  the property name column, the user can press keys to do the
// live serach.
// Check TzObjectInspectorEdit.WMKillFocus, and TzCustomObjectInspector.WMHotKey
procedure TzObjectInspectorEdit.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;
  TzCustomObjectInspector(FOwnerInspector).RegisterKeys;
end;

procedure TzObjectInspectorEdit.WMWindowPosChanged(var AMessage: TWMWindowPosChanged);
begin
  inherited;
  UpdateButton;
end;

procedure TzObjectInspectorEdit.WndProc(var AMessage: TMessage);
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

constructor TzFloatPreference.Create;
const
  defExpPrecision = 2;
  defMaxDigits = 6;
begin
  inherited;
  FExpPrecision := defExpPrecision;
  FMaxDigits := defMaxDigits;
end;

procedure TzFloatPreference.Assign(ASource: TPersistent);
begin
  if ASource is TzFloatPreference then begin
    MaxDigits := TzFloatPreference(ASource).MaxDigits;
    ExpPrecision := TzFloatPreference(ASource).ExpPrecision;
  end
  else begin
    inherited Assign(ASource);
  end;
end;

procedure TzPropertyItem.CheckOwnerList;
begin
  if not Assigned(FOwnerList) then raise EItemOwnerListNotAssigned.Create;
end;

class function TzPropertyItem.Empty: TzPropertyItem;
begin
  ZeroMemory(@Result, SizeOf(TzPropertyItem));
end;

function TzPropertyItem.EqualTo(AItemToCompare: PzPropertyItem): Boolean;
begin
  Result := @Self = AItemToCompare;
end;

function TzPropertyItem.get_AssociatedComponentParentForm: TCustomForm;
begin
  Result := GetComponentParentForm(Component);
end;

function TzPropertyItem.get_AssociatedProperty: TRttiProperty;
begin
  Result := FAssociatedProperty;
end;

procedure TzPropertyItem.set_AssociatedProperty(const AValue: TRttiProperty);
begin
  FAssociatedProperty := AValue;
end;

function TzPropertyItem.get_CategoryIndex: Integer;
begin
  Result := FCategoryIndex;
end;

procedure TzPropertyItem.set_CategoryIndex(const AValue: Integer);
begin
  FCategoryIndex := AValue;
end;

function TzPropertyItem.get_CategoryName: string;
begin
  Result := FCategoryName;
end;

procedure TzPropertyItem.set_CategoryName(const AValue: string);
begin
  FCategoryName := AValue;
end;

function TzPropertyItem.get_ChildCount: Integer;
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
        if FOwnerList.Items[I].CategoryIndex = CategoryIndex then begin
          Inc(Result)
        end
        else begin
          if FOwnerList.Items[I].CategoryIndex <> -1 then Break;
        end
      end
      else begin
        // Check parents to get child!
        if FOwnerList.Items[I].Parent = @Self then begin
          Inc(Result)
        end
        else begin
          if FOwnerList.Items[I].Parent = Parent then Break;
        end
      end;
    end;
  end;
end;

function TzPropertyItem.get_ChildItems(const AIndex: Integer): PzPropertyItem;
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
        end
        else begin
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

function TzPropertyItem.get_Component: TObject;
begin
  Result := FComponent;
end;

procedure TzPropertyItem.set_Component(const AValue: TObject);
begin
  FComponent := AValue;
end;

function TzPropertyItem.get_Expanded: Boolean;
begin
  CheckOwnerList;
  Result := ChildCount > 0;
  if Result then Result := Assigned(ChildItems[0]);
  if Result then Result := ChildItems[0].Visible;
end;

procedure TzPropertyItem.set_FloatPreference(const AValue: TzFloatPreference);
begin
  FFloatPreference := AValue;
end;

function TzPropertyItem.get_HasChild: Boolean;
begin
  if IsCategory then
    Exit(True);

  if (not MayHaveChild) then
    Exit(False);   // Category, Set, and Class.

  Result := not Value.IsEmpty;

  if Result and IsClass then
    Result := ObjectHasAtLeastOneChild(Value.AsObject, ObjectVisibility);
end;

function TzPropertyItem.get_Instance: TObject;
begin
  Result := FInstance;
end;

procedure TzPropertyItem.set_Instance(const AValue: TObject);
begin
  FInstance := AValue;
end;

function TzPropertyItem.get_IsCategory: Boolean;
begin
  Result := FIsCategory;
end;

procedure TzPropertyItem.SetIsCategory(const AValue: Boolean);
begin
  FIsCategory := AValue;
end;

function TzPropertyItem.get_IsClass: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := AssociatedProperty.PropertyType.TypeKind = tkClass;
end;

function TzPropertyItem.get_IsComponent: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkClass) and
    IsClassDerivedFromClass(TRttiInstanceType(AssociatedProperty.PropertyType).MetaclassType, TComponent);
end;

function TzPropertyItem.get_IsEmpty: Boolean;
var
  emptyItem: TzPropertyItem;
begin
  emptyItem := TzPropertyItem.Empty;
  Result := CompareMem(@Self, @emptyItem, SizeOf(TzPropertyItem));
end;

function TzPropertyItem.get_IsEnum: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := AssociatedProperty.PropertyType.TypeKind = tkEnumeration;
end;

function TzPropertyItem.get_IsSet: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkSet) and (SetElementValue = - 1);
end;

function TzPropertyItem.get_IsSetElement: Boolean;
begin
  if (not Assigned(AssociatedProperty)) or (IsCategory) then
    Exit(False);

  Result := (AssociatedProperty.PropertyType.TypeKind = tkSet) and (SetElementValue > - 1);
end;

function TzPropertyItem.get_MayHaveChild: Boolean;
begin
  Result := IsCategory or IsClass or IsSet;
end;

function TzPropertyItem.get_Name: string;
begin
  if IsCategory then begin
    Result := CategoryName;
    Exit;
  end;

  if get_IsSetElement then
    Result := GetEnumName(AssociatedProperty.PropertyType.AsSet.ElementType.Handle, SetElementValue)
  else
    Result := AssociatedProperty.Name;
end;

function TzPropertyItem.get_ObjectVisibility: TMemberVisibility;
begin
  Result := FObjectVisibility;
end;

procedure TzPropertyItem.set_ObjectVisibility(const AValue: TMemberVisibility);
begin
  FObjectVisibility := AValue;
end;

function TzPropertyItem.get_Parent: PzPropertyItem;
begin
  Result := FParent;
end;

procedure TzPropertyItem.set_Parent(const AValue: PzPropertyItem);
begin
  FParent := AValue;
end;

function TzPropertyItem.get_QualifiedName: string;
begin
  Result := FQualifiedName;
end;

procedure TzPropertyItem.set_QualifiedName(const AValue: string);
begin
  FQualifiedName := AValue;
end;

function TzPropertyItem.get_SetElementValue: Integer;
begin
  Result := FSetElementValue;
end;

procedure TzPropertyItem.set_SetElementValue(const AValue: Integer);
begin
  FSetElementValue := AValue;
end;

function TzPropertyItem.get_Value: TValue;
begin
  Result := TValue.Empty;

  if IsCategory then
    Exit;

  if Assigned(Instance) then
    Result := AssociatedProperty.GetValue(Instance);
end;

function TzPropertyItem.get_ValueAsString: String;
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

function TzPropertyItem.get_Visible: Boolean;
begin
  Result := FVisible;
end;

procedure TzPropertyItem.set_Visible(const AValue: Boolean);
begin
  FVisible := AValue;
end;

function TzPropertyItemList.IndexOfQualifiedName(AQualifiedName: String): Integer;
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
  Result := CompareText(PzPropertyItem(Item1)^.QualifiedName, PzPropertyItem(Item2)^.QualifiedName);
end;

procedure TzPropertyItemList.Sort;
begin
  inherited Sort(@Compare);
end;

constructor TzCanvasStack.Create;
begin
  inherited Create;
  FObjectsStack := TStack<ICanvasObjectsStore>.Create;
end;

constructor TzCanvasStack.Create(const ACollection: TEnumerable<TCanvas>);
begin
  inherited Create(ACollection);
  FObjectsStack := TStack<ICanvasObjectsStore>.Create;
end;

destructor TzCanvasStack.Destroy;
begin
  FObjectsStack.Free;
  inherited;
end;

procedure TzCanvasStack.Clear;
begin
  inherited Clear;
  FObjectsStack.Clear;
end;

function TzCanvasStack.Extract: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Extract;
  canvasObjectsStore := FObjectsStack.Extract;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

function TzCanvasStack.Peek: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Peek;
  canvasObjectsStore := FObjectsStack.Peek;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

function TzCanvasStack.Pop: TCanvas;
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  Result := inherited Pop;
  canvasObjectsStore := FObjectsStack.Pop;
  canvasObjectsStore.RestoreCanvasObjects(Result);
end;

procedure TzCanvasStack.Push(const ACanvas: TCanvas);
var
  canvasObjectsStore: ICanvasObjectsStore;
begin
  inherited Push(ACanvas);
  canvasObjectsStore := TCanvasObjectsStore.Create;
  canvasObjectsStore.SaveCanvasObjects(ACanvas);
  FObjectsStack.Push(canvasObjectsStore);
end;

procedure TzCanvasStack.TrimExcess;
begin
  inherited TrimExcess;
  FObjectsStack.TrimExcess;
end;

constructor TzCanvasStack.TCanvasObjectsStore.Create;
begin
  inherited Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
end;

destructor TzCanvasStack.TCanvasObjectsStore.Destroy;
begin
  FPen.Free;
  FFont.Free;
  FBrush.Free;
  inherited;
end;

procedure TzCanvasStack.TCanvasObjectsStore.RestoreCanvasObjects(ACanvas: TCanvas);
begin
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Font.Assign(FFont);
  ACanvas.Pen.Assign(FPen);
  ACanvas.Refresh;
end;

procedure TzCanvasStack.TCanvasObjectsStore.SaveCanvasObjects(ACanvas: TCanvas);
begin
  FBrush.Assign(ACanvas.Brush);
  FFont.Assign(ACanvas.Font);
  FPen.Assign(ACanvas.Pen);
end;

function TzRttiType.GetUsedProperties: TArray<TRttiProperty>;
var
  prop: TRttiProperty;
  propArray: TArray<TRttiProperty>;
  propArrayLen: Integer;
  {$REGION 'Local Function: Contains'}
  function Contains(APropArray: TArray<TRttiProperty>; AProp: TRttiProperty): Boolean;
  var
    lfProp: TRttiProperty;
  begin
    Result := False;
    for lfProp in APropArray do
      if lfProp.Name = AProp.Name then
        Exit(True);
  end;
{$ENDREGION}
begin
  Result := GetDeclaredProperties;
  propArray := GetProperties;

  for prop in propArray do
  begin
    if Assigned(prop) and (not Contains(Result, prop)) then begin
      propArrayLen := Length(Result) + 1;
      SetLength(Result, propArrayLen);
      Result[propArrayLen - 1] := prop;
    end;
  end;
end;

constructor TzObjectHost.Create;
begin
  FList := TList<TzObjectNamePair>.Create;
end;

destructor TzObjectHost.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TzObjectHost }
procedure TzObjectHost.AddObject(AObject: TObject; const AName: string);
var
  objNamePair: TzObjectNamePair;
begin
  objNamePair.Key := AObject;
  objNamePair.Value := AName;
  FList.Add(objNamePair);
end;

function TzObjectHost.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TzObjectHost.GetItem(AIndex: Integer): TzObjectNamePair;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FList[AIndex];
end;

constructor TzRecordList<T, PT>.Create;
begin
  ValidateTypeParameters;
  FRecPtrList := TList.Create;
end;

destructor TzRecordList<T, PT>.Destroy;
begin
  Clear;
  FRecPtrList.Free;
  inherited;
end;

function TzRecordList<T, PT>.Add(const ARecordValue: T): Integer;
var
  PRec: PByte;
begin
  PtrT(PTToPointer((Add)))^ := ARecordValue;
  Result := Count - 1;
end;

function TzRecordList<T, PT>.Add: PT;
begin
  Result := PointerToPT(AllocMem(SizeOf(T)));
  FRecPtrList.Add(PTToPointer(Result));
end;

procedure TzRecordList<T, PT>.Clear;
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

procedure TzRecordList<T, PT>.Delete(const AIndex: Integer);
var
  ptr: Pointer;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  ptr := FRecPtrList.Items[AIndex];
  FreeRecord(ptr);
  FRecPtrList.Delete(AIndex);
end;

function TzRecordList<T, PT>.First: PT;
begin
  Result := PT(nil);

  if Count > 0 then
    Result := get_Item(0);
end;

function TzRecordList<T, PT>.Last: PT;
begin
  Result := PT(nil);

  if Count > 0 then
    Result := get_Item(Count - 1);
end;

function TzRecordList<T, PT>.PointerToPT(APointer: Pointer): PT;
var
  temp: PT absolute APointer;
begin
  Result := temp;
end;

// absolute directive: declare a variable that resides at the same address as another variable.
function TzRecordList<T, PT>.PTToPointer(ARecord: PT): Pointer;
var
  temp: Pointer absolute ARecord;
begin
  Result := temp;
end;

procedure TzRecordList<T, PT>.Sort(ACompare: TListSortCompare);
begin
  FRecPtrList.Sort(ACompare);
end;

procedure TzRecordList<T, PT>.FreeRecord(APointer: Pointer);
begin
  if Assigned(APointer) then
  begin
    Finalize(PtrT(APointer)^);
    FreeMem(APointer, SizeOf(T));
  end;
end;

function TzRecordList<T, PT>.get_Item(const AIndex: Integer): PT;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := PointerToPT(FRecPtrList.Items[AIndex]);
end;

function TzRecordList<T, PT>.get_Count: Integer;
begin
  Result := FRecPtrList.Count;
end;

function TzRecordList<T, PT>.IndexOf(ARecordPtr: PT): Integer;
begin
  Result := FRecPtrList.IndexOf(PTToPointer(ARecordPtr));
end;

class procedure TzRecordList<T, PT>.ValidateTypeParameters;
const
  cInvalidTypeParam_1 = 'TzRecordList<T,PT> type param_1 has invalid type <%s>. It must be a record.';
  cInvalidTypeParam_2 = 'TzRecordList<T,PT> type param_2 has invalid type <%s>. It must be a pointer to %s.';
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

function TzCustomControl.get_IsMouseDown: Boolean;
begin
  Result := FIsMouseLButtonDown;
end;

function TzCustomControl.get_IsMouseInControl: Boolean;
var
  screenPos, clientPos: TPoint;
begin
  GetCursorPos(screenPos);
  clientPos := ScreenToClient(screenPos);
  Result := ClientRect.Contains(clientPos);
end;

function TzCustomControl.get_IsVclStyleUsed: Boolean;
begin
  Result := StyleServices.Enabled and not StyleServices.IsSystemStyle;
end;

function TzCustomControl.get_UseStyleBorder: Boolean;
begin
  Result := IsVclStyleUsed and (seBorder in StyleElements);
end;

function TzCustomControl.get_UseStyleColor: Boolean;
begin
  Result := IsVclStyleUsed and (seClient in StyleElements);
end;

function TzCustomControl.get_UseStyleFont: Boolean;
begin
  Result := IsVclStyleUsed and (seFont in StyleElements);
end;

procedure TzCustomControl.WMLButtonDown(var AMessage: TWMLButtonDown);
begin
  FIsMouseLButtonDown := True;
  inherited;
end;

procedure TzCustomControl.WMLButtonUp(var AMessage: TWMLButtonUp);
begin
  FIsMouseLButtonDown := False;
  inherited;
end;

{ TzObjInspectorBase }
constructor TzObjectInspectorBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLockUpdate := False;
  FCanvasStack := TzCanvasStack.Create;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FContext := TRttiContext.Create;
  FItems := TzPropertyItemList.Create;
  FVisibleItems := TList<PzPropertyItem>.Create;
  FExpandedList := TList<string>.Create;
  FSaveVisibleItems := TList<string>.Create;
  FCircularLinkedProperties := TList<string>.Create;
  FDefaultPropertyValueMap := TDictionary<string, string>.Create;
  FCategories := TList<string>.Create;
  FPropertyCategoryMap := TDictionary<string, Integer>.Create;
  FPropertyInstances := TDictionary<string, TObject>.Create;
  FDefaultCategoryName := ValueManager.DefaultCategoryName;
  FSortByCategory := False;
  FOnBeforeAddItem := nil;
  FComponent := nil;
  FObjectVisibility := mvPublic;
  FFloatPreference := TzFloatPreference.Create;
end;

destructor TzObjectInspectorBase.Destroy;
begin
  FCanvasStack.Free;
  FFloatPreference.Free;
  FContext.Free;

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

  if Assigned(FComponent) and (FComponent is TzObjectHost) then
    FComponent.Free;

  inherited;
end;

procedure TzObjectInspectorBase.BeginUpdate;
begin
  FLockUpdate := True;
end;

procedure TzObjectInspectorBase.Changed;
begin
  UpdateProperties(True);
end;

function TzObjectInspectorBase.CircularLinkedProperties: TList<string>;
begin
  Result := FCircularLinkedProperties;
end;

procedure TzObjectInspectorBase.ClearRegisteredCategorys;
begin
  FCategories.Clear;
  FPropertyCategoryMap.Clear;
  FCategories.Add(FDefaultCategoryName);
  UpdateProperties(True);
end;

procedure TzObjectInspectorBase.ComponentChanged;
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

procedure TzObjectInspectorBase.EndUpdate;
begin
  if FLockUpdate then begin
    FLockUpdate := False;
    Invalidate;
  end;
end;

function TzObjectInspectorBase.ExpandedList: TList<string>;
begin
  Result := FExpandedList;
end;

procedure TzObjectInspectorBase.Invalidate;
begin
  if not FLockUpdate then begin
    inherited;
    CanvasStack.Clear;
    CanvasStack.TrimExcess;
    inherited;
  end;
end;

function TzObjectInspectorBase.IsItemCircularLink(AItem: PzPropertyItem): Boolean;
begin
  Result := FCircularLinkedProperties.Contains(AItem^.QualifiedName);
end;

function TzObjectInspectorBase.IsValueNotDefault(AQualifiedName: string; AValue: string): Boolean;
begin
  Result := False;

  if FDefaultPropertyValueMap.ContainsKey(AQualifiedName) then
    Result := FDefaultPropertyValueMap[AQualifiedName] <> AValue;
end;

function TzObjectInspectorBase.ItemNeedUpdate(AItem: PzPropertyItem): Boolean;
  function wasModified(AItemToTest: PzPropertyItem): Boolean;
  begin
    Result := AItemToTest.IsClass and (FPropertyInstances.ContainsKey(AItemToTest.QualifiedName)) and
      (FPropertyInstances[AItemToTest.QualifiedName] <> AItemToTest.Value.AsObject);
  end;
var
  parentItem: PzPropertyItem;
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

function TzObjectInspectorBase.ItemOrder(PItem: PzPropertyItem): Integer;
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

function TzObjectInspectorBase.LockUpdate: Boolean;
begin
  Result := FLockUpdate;
end;

function TzObjectInspectorBase.NeedUpdate: Boolean;
var
  i: Integer;
  PItem: PzPropertyItem;
begin
  Result := False;

  for i := 0 to FVisibleItems.Count - 1 do begin
    PItem := FVisibleItems[i];

    if ItemNeedUpdate(PItem) then
      Exit(True);
  end;
end;

procedure TzObjectInspectorBase.RegisterPropertyInCategory(const ACategoryName, APropertyName: string);
var
  categoryIndex: Integer;
begin
  categoryIndex := FCategories.IndexOf(ACategoryName);

  if categoryIndex < 0 then
    categoryIndex := FCategories.Add(ACategoryName);

  if not FPropertyCategoryMap.ContainsKey(APropertyName) then
    FPropertyCategoryMap.Add(APropertyName, categoryIndex);
end;

function TzObjectInspectorBase.SaveVisibleItems: TList<string>;
begin
  Result := FSaveVisibleItems;
end;

procedure TzObjectInspectorBase.UpdateVisibleItems;
var
  I: Integer;
  item: PzPropertyItem;
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
    end
    else begin
      visibleCondition := (item.ChildCount > 0) and FIsSettingComponent and Assigned(OnAutoExpandItemOnInit)
        and OnAutoExpandItemOnInit(Self, item);

      if visibleCondition then
        makeChildrenVisible;
    end;

    if item.Visible or FSaveVisibleItems.Contains(item.QualifiedName) then
      FVisibleItems.Add(item);
  end;
end;

function TzObjectInspectorBase.VisibleItems: TList<PzPropertyItem>;
begin
  Result := FVisibleItems;
end;

procedure TzObjectInspectorBase.set_Component(AValue: TObject);
begin
  if AValue <> FComponent then begin
    FIsSettingComponent := True;
    try
      if Assigned(FComponent) and (FComponent is TzObjectHost) then
        FreeAndNil(FComponent);

      FComponent := AValue;
      ComponentChanged;
    finally
      FIsSettingComponent := False;
    end;
  end;
end;

function TzObjectInspectorBase.get_FloatPreference: TzFloatPreference;
begin
  Result := FFloatPreference;
end;

procedure TzObjectInspectorBase.set_ObjectVisibility(const AValue: TMemberVisibility);
begin
  if AValue >= mvPublic then
    FObjectVisibility := AValue
  else
    raise EInvalidObjectVisibility.Create;
end;

procedure TzObjectInspectorBase.set_SortByCategory(const AValue: Boolean);
begin
  if FSortByCategory <> AValue then begin
    FSortByCategory := AValue;
    UpdateProperties(True);
  end;
end;

procedure TzObjectInspectorBase.UpdateItems;
var
  categories: TList<string>;
  component: TObject;
  componentName: string;
  I: Integer;
  isMultiInstance: Boolean;
  objHost: TzObjectHost;
  {$REGION 'EnumProps'}
  procedure EnumProps(AInstance: TObject; AParentItem, ACategoryItem: PzPropertyItem; AQualifiedName, AQualifiedType: string);
  var
    lvAllow: Boolean;
    lvCategoryIndex: Integer;
    lvCategoryItem: PzPropertyItem;
    lvCategoryName: string;
    lvInstance: TObject;
    lvItem: PzPropertyItem;
    lvQName, lvQType: string;
    lvRttiProperty: TRttiProperty;
    lvRttiPropertyArray: TArray<TRttiProperty>;
    {$REGION 'AddNewCategory'}
    function AddNewCategory: PzPropertyItem;
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
      llvSetItem: PzPropertyItem;
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
    lvRttiPropertyArray := TzRttiType(FRttiType).GetUsedProperties;

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
        end
        else begin
          lvCategoryIndex := FCategories.Add(lvCategoryName);
          lvCategoryItem := AddNewCategory;
        end;
      end
      else begin
        if FSortByCategory and (AInstance = component) then begin
          if FPropertyCategoryMap.ContainsKey(lvRttiProperty.Name) then begin
            lvCategoryIndex := FPropertyCategoryMap[lvRttiProperty.Name];
            lvCategoryName := FCategories[lvCategoryIndex];
            lvQName := lvCategoryName + '.' + lvQName;
          end
          else begin
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
      end
      else begin
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

    if FComponent is TzObjectHost then begin
      objHost := TzObjectHost(FComponent);
      FSortByCategory := True;
      isMultiInstance := True;
      FCategories.Clear;
      FPropertyCategoryMap.Clear;

      for I := 0 to objHost.Count - 1 do begin
        component := objHost.Item[I].Key;
        componentName := objHost.Item[I].Value;
        EnumProps(component, nil, nil, componentName + '.' + component.Tostring, component.Tostring);
      end;
    end
    else begin
      EnumProps(FComponent, nil, nil, FComponent.Tostring, FComponent.Tostring);
    end;

    FItems.Sort;
  finally
    FreeAndNil(categories);
  end
end;

constructor TzObjectInspectorList.Create(AOwner: TComponent);
const
  cDefaultItemHeight = 17;
  cDefaultWidth = 300;
  cDefaultHeight = 300;
begin
  inherited;
  Width := cDefaultWidth;
  Height := cDefaultHeight;
  FBorderStyle := bsSingle;
  FReadOnly := False;
  FItemHeight := cDefaultItemHeight;
end;

destructor TzObjectInspectorList.Destroy;
begin
  inherited;
end;

procedure TzObjectInspectorList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params); // inherited from TWinControl

  if FBorderStyle <> bsNone then
    Params.Style := Params.Style or WS_BORDER;
end;

procedure TzObjectInspectorList.set_BorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd; // TWinControl.RecreateWnd
  end;
end;

{ TzObjInspectorSizing }
constructor TzObjectInspectorSplitterList.Create(AOwner: TComponent);
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

destructor TzObjectInspectorSplitterList.Destroy;
begin
  inherited;
end;

procedure TzObjectInspectorSplitterList.DrawSplitter(ACanvas: TCanvas);
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

procedure TzObjectInspectorSplitterList.InvalidateNC;
begin
  // The WM_NCPAINT message is sent to a window when its frame must be painted.
  if HandleAllocated and not LockUpdate then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TzObjectInspectorSplitterList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TzObjectInspectorSplitterList.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if csDesigning in ComponentState then
    Exit;

  FSplitterDown := False;
end;

procedure TzObjectInspectorSplitterList.Paint;
begin
  inherited;
  DrawSplitter(Canvas);
end;

procedure TzObjectInspectorSplitterList.SplitterPosChanged(var ANewPos: Integer);
begin
  if Assigned(FOnSplitterPosChanged) then
    FOnSplitterPosChanged(Self, ANewPos);
end;

procedure TzObjectInspectorSplitterList.WMMouseMove(var AMessage: TWMMouseMove);
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

procedure TzObjectInspectorSplitterList.set_SplitterColor(const AValue: TColor);
begin
  if AValue <> FSplitterColor then begin
    FSplitterColor := AValue;
    Invalidate;
  end;
end;

procedure TzObjectInspectorSplitterList.set_SplitterPos(const AValue: Integer);
const
  cSplitterPosMargin = 10;
begin
  if (FSplitterPos <> AValue) and (AValue > cSplitterPosMargin) and (AValue < ClientWidth - cSplitterPosMargin) then begin
    FSplitterPos := AValue;
    Invalidate;
  end;
end;

function TzObjectInspectorSplitterList.get_SplitterRect: TRect;
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

constructor TzObjectInspectorHeaderList.Create(AOwner: TComponent);
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

procedure TzObjectInspectorHeaderList.Paint;
begin
  if FShowHeader then PaintHeader;
  inherited;
end;

procedure TzObjectInspectorHeaderList.PaintHeader;
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

procedure TzObjectInspectorHeaderList.WMLButtonDown(var AMessage: TWMLButtonDown);
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
      end
      else begin
        if FHeaderValuePressed then
          FOnHeaderMouseDown(Self, hpRight, pt.X, pt.Y);
      end;
    end;
  end;
end;

procedure TzObjectInspectorHeaderList.WMLButtonUp(var AMessage: TWMLButtonUp);
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

function TzObjectInspectorHeaderList.get_HeaderPropRect: TRect;
begin
  Result := Rect(0, 0, SplitterPos, HeaderRect.Height);
end;

procedure TzObjectInspectorHeaderList.set_HeaderPropText(const AValue: string);
begin
  if FHeaderPropText <> AValue then begin
    FHeaderPropText := AValue;
    Invalidate;
  end;
end;

function TzObjectInspectorHeaderList.get_HeaderRect: TRect;
begin
  Result := Rect(0, 0, Width, ItemHeight + (ItemHeight div 2));
end;

function TzObjectInspectorHeaderList.get_HeaderValueRect: TRect;
begin
  Result := Rect(SplitterPos, 0, Width, HeaderRect.Height);
end;

procedure TzObjectInspectorHeaderList.set_HeaderValueText(const AValue: string);
begin
  if FHeaderValueText <> AValue then begin
    FHeaderValueText := AValue;
    Invalidate;
  end;
end;

procedure TzObjectInspectorHeaderList.set_ShowHeader(const AValue: Boolean);
begin
  if FShowHeader <> AValue then begin
    FShowHeader := AValue;
    Invalidate;
  end;
end;

constructor TzObjectInspectorScrollList.Create(AOwner: TComponent);
begin
  inherited;
  FPrevScrollPos := 0;
end;

destructor TzObjectInspectorScrollList.Destroy;
begin

  inherited;
end;

procedure TzObjectInspectorScrollList.CMFontChanged(var AMessage: TMessage);
const
  cDummyText = 'WA';
  cItemHeightMargin = 4;
begin
  inherited;
  Canvas.Font.Assign(Font);
  ItemHeight := Canvas.TextHeight(cDummyText) + cItemHeightMargin; // 17;
end;

procedure TzObjectInspectorScrollList.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  AParams.Style := AParams.Style or WS_VSCROLL;
end;

function TzObjectInspectorScrollList.FirstItemIndex: Integer;
begin
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_POS;
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  Result := Max(0, FScrollInfo.nPos);
end;

function TzObjectInspectorScrollList.IndexFromPoint(APoint: TPoint): Integer;
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

function TzObjectInspectorScrollList.ItemTop(AIndex: Integer): Integer;
begin
  Result := AIndex * ItemHeight;

  if ShowHeader then
    Inc(Result, HeaderRect.Height);
end;

function TzObjectInspectorScrollList.LastItemIndex: Integer;
begin
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.fMask := SIF_POS;
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  Result := min(VisiblePropCount - 1, FScrollInfo.nPos + MaxItemCount);
end;

function TzObjectInspectorScrollList.MaxItemCount: Integer;
var
  LHeight: Integer;
begin
  LHeight := Height;

  if ShowHeader then
    Dec(LHeight, HeaderRect.Height);

  Result := LHeight div ItemHeight;
end;

function TzObjectInspectorScrollList.IndexToVirtualIndex(AIndex: Integer): Integer;
begin
  Result := AIndex - FirstItemIndex;
end;

procedure TzObjectInspectorScrollList.Paint;
var
  i: Integer;
  FirstItem: Integer;
  LastItem: Integer;
begin
  PaintBackground(Canvas);
  FirstItem := FirstItemIndex;
  LastItem := LastItemIndex;

  for i := FirstItem to LastItem do begin
    CanvasStack.Push(Canvas);
    PaintItem(i);
    CanvasStack.Pop;
  end;

  inherited;
end;

procedure TzObjectInspectorScrollList.PaintBackground(ACanvas: TCanvas);
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

procedure TzObjectInspectorScrollList.PaintItem(AIndex: Integer);
begin
  { ==> Override <== }
end;

procedure TzObjectInspectorScrollList.UpdateScrollBar;
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

procedure TzObjectInspectorScrollList.WMEraseBkgnd(var AMessage: TWMEraseBkgnd);
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
procedure TzObjectInspectorScrollList.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  AMessage.Result := DLGC_WANTARROWS;
end;

procedure TzObjectInspectorScrollList.WMSize(var AMessage: TWMSize);
begin
  inherited;
  UpdateScrollBar;
end;

procedure TzObjectInspectorScrollList.WMVScroll(var AMessage: TWMVScroll);
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
      i: Integer;
      L: Integer;
      Child: TControl;
    begin
      { Manually Scroll Childs when ScrollFlags <> SW_SCROLLCHILDREN ! }
      for i := 0 to Self.ControlCount - 1 do begin
        Child := Self.Controls[i];

        if not (Child is TzObjectInspectorButton) and (Child.Visible) then begin
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
    end
    else begin
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
        end
        else begin
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

procedure TzObjectInspectorScrollList.WMWindowPosChanged(var AMessage: TWMWindowPosChanged);
begin
  inherited;
  UpdateScrollBar;
end;

function TzObjectInspectorScrollList.get_VisiblePropCount: Integer;
begin
  Result := VisibleItems.Count;
end;

constructor TzCustomObjectInspector.Create(AOwner: TComponent);
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
  FSelItem := TzPropertyItem.Empty;
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
  FPropInspEdit := TzObjectInspectorEdit.Create(Self);
  FPropInspEdit.Visible := False;

  if not (csDesigning In ComponentState) then
    FPropInspEdit.Parent := Self;

  FPropInspEdit.BorderStyle := bsNone;
end;

destructor TzCustomObjectInspector.Destroy;
begin
  inherited;
end;

{ TzCustomObjInspector }

function TzCustomObjectInspector.CanDrawChevron(Index: Integer): Boolean;
var
  PItem: PzPropertyItem;
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

procedure TzCustomObjectInspector.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if (M <> D) then begin
    FGutterWidth := MulDiv(FGutterWidth, M, D);
    SplitterPos := MulDiv(SplitterPos, M, D);

    ValueManager.MinimumPlusSignWidth := MulDiv(10, M, D);
    ValueManager.ScaledColorRectWidth := MulDiv(ValueManager.DefaultColorRectWidth, M, D);

    TzObjectInspectorButton.ScaledWidth     := MulDiv(TzObjectInspectorButton.DefaultWidth, M, D);
    TzObjectInspectorButton.ScaledArrowSize := MulDiv(TzObjectInspectorButton.DefaultArrowSize, M, D);

    FSepTxtDis := MulDiv(FSepTxtDis, M, D);
  end;

  inherited ChangeScale(M, D, isDpiChange);
end;

procedure TzCustomObjectInspector.CMHintShow(var Message: TCMHintShow);
begin
  if FIsItemHint and FShowItemHint then begin
    Message.HintInfo.HintPos := FHintPoint;
    Message.HintInfo.HintWindowClass := TzObjectInspectorItemHintWindow;
    Message.HintInfo.HintData := Pointer(FBoldHint);
  end
  else begin
    inherited;
  end;

  FIsItemHint := False;
end;

procedure TzCustomObjectInspector.CMSTYLECHANGED(var Message: TMessage);
begin
  inherited;
  FSelectedIndex := - 1;
  UpdateScrollBar;
  UpdateEditControl(False);
end;

procedure TzCustomObjectInspector.CollapseAll;
begin
  SaveVisibleItems.Clear;
  ExpandedList.Clear;
  UpdateProperties(True);
end;

function TzCustomObjectInspector.CollapseItem(PItem: PzPropertyItem): Boolean;
begin
  Result := DoCollapseItem(PItem);

  if Result then
    UpdateProperties(True);
end;

procedure TzCustomObjectInspector.CreateWnd;
begin
  inherited;
  FSelectedIndex := - 1;
end;

function TzCustomObjectInspector.DoCollapseItem(PItem: PzPropertyItem): Boolean;
var
  i: Integer;
  PChild: PzPropertyItem;
begin
  Result := PItem^.HasChild;

  if not Result then
    Exit;

  if Assigned(FOnCollapseItem) then if not FOnCollapseItem(Self, PItem) then
    Exit(False);

  Result := False; // Indicate that item is already Collapsed !

  for i := 0 to PItem.ChildCount - 1 do begin
    PChild := PItem.ChildItems[i];

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

function TzCustomObjectInspector.DoExpandItem(PItem: PzPropertyItem): Boolean;
var
  i: Integer;
  PChild: PzPropertyItem;
  procedure MakeChildsVisible(PParent: PzPropertyItem; Visible: Boolean);
  var
    J: Integer;
    P: PzPropertyItem;
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
  Result := PItem.HasChild;

  if not Result then
    Exit;

  if CircularLinkedProperties.Contains(PItem.QualifiedName) then
    Exit(False);

  if Assigned(FOnExpandItem) then if not FOnExpandItem(Self, PItem) then
    Exit(False);

  Result := False; // Indicate that item is already Expanded !

  if not ExpandedList.Contains(PItem^.QualifiedName) then
    ExpandedList.Add(PItem^.QualifiedName);

  if not SaveVisibleItems.Contains(PItem^.QualifiedName) then
    SaveVisibleItems.Add(PItem^.QualifiedName);

  for i := 0 to PItem^.ChildCount - 1 do begin
    PChild := PItem^.ChildItems[i];

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

procedure TzCustomObjectInspector.DoExtraRectClick;
var
  PItem: PzPropertyItem;
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

function TzCustomObjectInspector.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
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

function TzCustomObjectInspector.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
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

function TzCustomObjectInspector.DoSelectCaret(Index: Integer): Boolean;
var
  X, Y, i, Offset: Integer;
  s: string;
begin
  Result := False;

  if (Index > - 1) and (Index < VisibleItems.Count - 1) then begin
    if CanFocus then begin
      SelectItem(Index);

      if GetFocus <> Handle then
        SetFocus;

      X := PropTextRect[Index].Left;
      Y := ItemRect[Index].Top;
      s := FSearchText;
      Offset := 0;

      if not s.IsEmpty then begin
        for i := 1 to Length(s) do Offset := Offset + Canvas.TextWidth(s[i]); { Calc caret pos }
      end;

      SetCaretPos(X + Offset - 1, Y + 1);
      ShowCaret(Handle);
      Result := True;
    end;
  end;
end;

function TzCustomObjectInspector.DoSetValue(PropItem: PzPropertyItem; var Value: TValue): Boolean;
begin
  Result := Assigned(PropItem);

  if not Result then
    Exit;

  if Assigned(FOnItemSetValue) then
    Result := FOnItemSetValue(Self, PropItem, Value);

  if Result then begin
    ValueManager.SetValue(PropItem, Value);

    if PropItem.IsClass then
      UpdateProperties(); { Must rebuild the list . }
  end;

  FPropInspEdit.UpdateEditText; // required on Result is True or False
  Invalidate;
end;

procedure TzCustomObjectInspector.ExpandAll;
var
  i: Integer;
begin
  ExpandedList.Clear;
  SaveVisibleItems.Clear;
  UpdateItems;

  for i := 0 to Items.Count - 1 do
    Items.Items[i].Visible := True;

  UpdateVisibleItems;
  UpdateScrollBar;
  UpdateEditControl(False);
  Invalidate;
end;

function TzCustomObjectInspector.ExpandItem(PItem: PzPropertyItem): Boolean;
begin
  Result := DoExpandItem(PItem);

  if Result then
    UpdateProperties(True);
end;

procedure TzCustomObjectInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  LSelectedItem: PzPropertyItem;
  LTxt: string;
  i: Integer;
  PItem: PzPropertyItem;
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

      for i := 0 to VisibleItems.Count - 1 do begin
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

procedure TzCustomObjectInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  P: TPoint;
  PItem: PzPropertyItem;
  MustUpdate: Boolean;
  SaveItem: TzPropertyItem;
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
      end
      else begin
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

procedure TzCustomObjectInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  w: Integer;
  P: TPoint;
  R: TRect;
  PItem: PzPropertyItem;
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
    end
    else begin
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

procedure TzCustomObjectInspector.Paint;
var
  i: Integer;
  PItem: PzPropertyItem;
  FirstIndex, LastIndex: Integer;
begin
  inherited;
  FirstIndex := FirstItemIndex;
  LastIndex := LastItemIndex;

  for i := FirstIndex to LastIndex do begin
    PItem := VisibleItems[i];
    if PItem^.IsCategory then
      PaintCategory(i);
  end;

  CanvasStack.TrimExcess;
end;

procedure TzCustomObjectInspector.PaintCategory(Index: Integer);
var
  PItem: PzPropertyItem;
  R: TRect;
  LDetails: TThemedElementDetails;
  LColor, LTxtColor: TColor;
begin
  CanvasStack.Push(Canvas);

  LDetails := StyleServices.GetElementDetails(tcbCategoryNormal);
  PItem := VisibleItems[Index];
  R := ItemRect[Index];
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

procedure TzCustomObjectInspector.PaintItem(Index: Integer);
var
  X, Y, cY: Integer;
  R, pmR: TRect;
  vIndex, POrd: Integer;
  PItem: PzPropertyItem;
  pOrdPos: Integer;
  DYT, DYB, PrevPos, NextPos: Integer;
  PropName: string;
  PPrevItem, PNextItem: PzPropertyItem;
  xMax, xMin: Integer;
  HasPlusMinus: Boolean;
  LSaveColor: TColor;
  LColor: TColor;
  HorzDotLeft: Integer;
begin

  if Index = FirstItemIndex then begin
    FPropsNeedHint := False;
    FValuesNeedHint := False;
  end;

  PItem := VisibleItems.Items[Index];
  vIndex := IndexToVirtualIndex(Index);
  HasPlusMinus := False;
  Y := ItemTop(vIndex);
  POrd := ItemOrder(PItem);
  pOrdPos := (POrd * FGutterWidth) + FGutterWidth;
  R := Rect(0, Y, pOrdPos, Y + ItemHeight);

  if Index = VisibleItems.Count - 1 then
    R.Height := Height;

  { Background color => will be used to paint property text . }
  LSaveColor := Canvas.Brush.Color;
  LColor := FGutterColor;

  if UseStyleColor then
    LColor := StyleServices.GetSystemColor(clBtnHighlight);

  Canvas.Brush.Color := LColor;
  Canvas.FillRect(R);
  pmR := PlusMinBtnRect[Index];

  if PItem^.HasChild and (not CircularLinkedProperties.Contains(PItem^.QualifiedName)) then begin
    DrawExpandbutton(Canvas, pmR.Left, pmR.Top, not PItem.Expanded, pmR.Width);
    HasPlusMinus := True;
  end;

  if not PItem^.IsCategory then begin
    if CanDrawChevron(Index) then begin
      cY := CenterPoint(pmR).Y - TzObjectInspectorButton.ScaledArrowSize;
      X := pOrdPos - (TzObjectInspectorButton.ScaledArrowSize * 2) - 1; // pOrdPos - (>>)-1
      // cY:=R.Top;
      if HasPlusMinus then
        Dec(X, ValueManager.MinimumPlusSignWidth + 2);

      Canvas.Pen.Color := clWindowText;

      if UseStyleColor then
        Canvas.Pen.Color := StyleServices.GetSystemColor(clWindowText);

      DrawChevron(Canvas, sdRight, Point(X, cY), TzObjectInspectorButton.ScaledArrowSize);
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

    if FSelectedIndex = Index then begin
      R := Rect(pOrdPos + 1, Y + 1, SplitterPos, Y + ItemHeight);
      LColor := FHighlightColor;

      if UseStyleColor then
        LColor := StyleServices.GetSystemColor(clHighlight);

      Canvas.Brush.Color := LColor;
      Canvas.FillRect(R);
    end
    else begin
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
    R := PropTextRect[Index];
    DrawText(Canvas.Handle, PropName, - 1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);

    if Canvas.TextWidth(PropName) > R.Width then
      FPropsNeedHint := True;

    Canvas.Brush.Color := LSaveColor;
    { ====> Paint Item Value <==== }
    PaintItemValue(PItem, Index);

    if Canvas.TextWidth(PItem^.ValueAsstring) > ValueTextRect[Index].Width then
      FValuesNeedHint := True;

    Canvas.Brush.Color := LSaveColor;
    Canvas.Pen.Color := FGridColor;
    HorzDotLeft := ValueManager.GetExtraRectWidth(PItem);

    if HorzDotLeft > 0 then
      HorzDotLeft := ValueTextRect[Index].Left
    else
      HorzDotLeft := SplitterPos;

    if (FSelectedIndex = Index) then begin
      DrawHorizontalDottedLine(Canvas, HorzDotLeft, Y, Width);
      DrawHorizontalDottedLine(Canvas, HorzDotLeft, Y + ItemHeight, Width);
    end
    else begin
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

  if (Index - 1) >= 0 then begin
    PPrevItem := VisibleItems.Items[Index - 1];
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
    end
    else begin
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

  if (Index + 1) < VisibleItems.Count then begin
    PNextItem := VisibleItems.Items[Index + 1];
    NextPos := (ItemOrder(PNextItem) * FGutterWidth) + FGutterWidth;
    //if pOrdPos <> NextPos then DYB := 2;
  end;

  Canvas.LineTo(pOrdPos, Y + ItemHeight - DYB);

  if (Index = VisibleItems.Count - 1) then begin
    Canvas.MoveTo(pOrdPos, Y + ItemHeight - DYB);
    Canvas.LineTo(pOrdPos, Height);
  end;
end;

procedure TzCustomObjectInspector.PaintItemValue(PItem: PzPropertyItem; Index: Integer);
  procedure doPaintItemValue(ACanvas: TCanvas; AIndex: Integer; const AItem: PzPropertyItem; ARect: TRect);
  var
    boolVal: Boolean;
    colorRectColor: TColor;
    DC: HDC;
    details: TThemedElementDetails;
    ExtraRect: TRect;
    Inspector: TzCustomObjectInspector;
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
    end
    else begin
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
  rect := ValueRect[Index];
  doPaintItemValue(Canvas, Index, PItem, rect);
  CanvasStack.Pop;
end;

procedure TzCustomObjectInspector.RegisterKeys;
begin
  if FAllowSearch and not (csDesigning in ComponentState) then
    FUnRegisterKeys := RegisterHotKey(Handle, 0, 0, VK_TAB)
end;

procedure TzCustomObjectInspector.SelectItem(Index: Integer);
var
  LSI: TScrollInfo;
  procedure DoSetScrollInfo;
  begin
    SetScrollInfo(Handle, SB_VERT, LSI, True);
    if UseStyleBorder then
      InvalidateNC;
  end;

begin
  if (Index < 0) then begin
    FSelectedIndex := - 1;
    FSelItem := TzPropertyItem.Empty;
    Invalidate;
    Exit;
  end;

  if (Index < VisibleItems.Count) then begin
    if (Index <> FSelectedIndex) then begin

      if Assigned(FOnSelectItem) then
        if not FOnSelectItem(Self, VisibleItems.Items[Index]) then
          Exit;

      LSI.cbSize := SizeOf(ScrollInfo);
      LSI.fMask := SIF_POS;
      FSelectedIndex := Index;

      if (FSelectedIndex < FirstItemIndex) then begin
        { Index out of page => Need to scroll ! }
        LSI.nPos := FSelectedIndex;
        DoSetScrollInfo;
      end
      else begin
        if (FSelectedIndex > LastItemIndex - 1) then begin
          { Index out of page => Need to scroll ! }
          LSI.nPos := 1 + FSelectedIndex - MaxItemCount;
          DoSetScrollInfo;
        end;
      end;

      FSelItem := VisibleItems[Index]^;
      Invalidate;
      UpdateEditControl;
      Exit;
    end;

    Exit;
  end;

  raise EIndexOutOfRange.Create;
end;

function TzCustomObjectInspector.SetPropValue(PropItem: PzPropertyItem; var Value: TValue): Boolean;
begin
  Result := DoSetValue(PropItem, Value);
end;

procedure TzCustomObjectInspector.SplitterPosChanged(var Pos: Integer);
begin
  if (Pos < FGutterWidth + 30) then
    Pos := FGutterWidth + 30;

  if (Pos > ClientWidth - 30) then
    Pos := ClientWidth  - 30;

  inherited SplitterPosChanged(Pos);
  UpdateEditControl;
end;

procedure TzCustomObjectInspector.UnRegisterKeys;
begin
  if not FUnRegisterKeys then
    Exit;

  UnregisterHotKey(Handle, 0);
  FUnRegisterKeys := False;
end;

procedure TzCustomObjectInspector.UpdateEditControl(const SetValue: Boolean);
var
  PItem: PzPropertyItem;
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

    if SetValue and FPropInspEdit.Visible and (Assigned(FPropInspEdit.PropertyItem)) then
      FPropInspEdit.SetValueFromEdit;

    FPropInspEdit.PropertyItem := PItem;
    //with ValueRect[FSelectedIndex] do
    begin
      if ValueManager.HasButton(PItem) then
        BtnWidth := TzObjectInspectorButton.ScaledWidth // 17
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

procedure TzCustomObjectInspector.UpdateProperties(const Repaint: Boolean);
begin
  UpdateItems;
  UpdateVisibleItems;
  UpdateScrollBar;
  UpdateSelIndex;
  UpdateEditControl(False);

  if Repaint then
    Invalidate;
end;

procedure TzCustomObjectInspector.UpdateSelIndex;
var
  P: PzPropertyItem;
begin
  P := SelectedItem;

  if Assigned(P) then begin
    FSelectedIndex := VisibleItems.IndexOf(P);
    SelectItem(FSelectedIndex);
  end;
end;

procedure TzCustomObjectInspector.WMHotKey(var Msg: TWMHotKey);
var
  parentForm: TCustomForm;
begin
  inherited;
  parentForm := GetComponentParentForm(Self);

  if Assigned(parentForm) then begin
    if FAllowSearch and (Msg.HotKey = 0) then begin// HotKey is the id of the Hot Key.
      if Assigned(parentForm.ActiveControl) then begin
        if (ContainsWindow(parentForm.ActiveControl.Handle, Handle)) then begin
          // ActiveControl.Handle is Edit, Handle is Inspector. GetCaretWnd will return the Edit
          // Handle too. So GetCaretWnd = Handle will never be true.
          if GetCaretWnd = Handle then begin // searching
            FSearchText := '';
            UpdateEditControl; // move back to Edit
          end
          else begin
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

procedure TzCustomObjectInspector.WMKILLFOCUS(var Msg: TWMKILLFOCUS);
begin
  if GetCaretWnd = Handle then
    DestroyCaret;
end;

procedure TzCustomObjectInspector.WMLBUTTONDBLCLK(var Message: TWMLBUTTONDBLCLK);
begin
  inherited;
end;

procedure TzCustomObjectInspector.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
end;

procedure TzCustomObjectInspector.WMLButtonUp(var Message: TWMLButtonUp);
var
  P: TPoint;
begin
  inherited;
  P := Point(Message.XPos, Message.YPos);

  if (FExtraRectIndex > - 1) and not (ExtraRect[FExtraRectIndex].IsEmpty) then begin
    if ExtraRect[FExtraRectIndex].Contains(P) then
      DoExtraRectClick;
  end;
end;

procedure TzCustomObjectInspector.WMSETFOCUS(var Msg: TWMSetFocus);
begin
  inherited;

  if FAllowSearch then
    CreateCaret(Handle, 0, 1, ItemHeight - 2);
end;

procedure TzCustomObjectInspector.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
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

procedure TzCustomObjectInspector.set_AllowSearch(const Value: Boolean);
begin
  if FAllowSearch <> Value then begin
    FAllowSearch := Value;
  end;
end;

procedure TzCustomObjectInspector.set_BoldNonDefaultValue(const Value: Boolean);
begin
  if Value <> FBoldNonDefaultValue then begin
    FBoldNonDefaultValue := Value;
    Invalidate;
  end;
end;

function TzCustomObjectInspector.get_ExtraRect(Index: Integer): TRect;
var
  w: Integer;
  PItem: PzPropertyItem;
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

procedure TzCustomObjectInspector.set_GridColor(const Value: TColor);
begin
  if FGridColor <> Value then begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_GutterColor(const Value: TColor);
begin
  if Value <> FGutterColor then begin
    FGutterColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_GutterEdgeColor(const Value: TColor);
begin
  if Value <> FGutterEdgeColor then begin
    FGutterEdgeColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_GutterWidth(const Value: Integer);
begin
  if Value > ValueManager.MaximumGutterWidth then
    raise EInvalidGutterWidth.Create;

  if FGutterWidth <> Value then begin
    FGutterWidth := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_HighlightColor(const Value: TColor);
begin
  if Value <> FHighlightColor then begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;

function TzCustomObjectInspector.get_ItemRect(Index: Integer): TRect;
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

procedure TzCustomObjectInspector.set_NameColor(const Value: TColor);
begin
  if Value <> FNameColor then begin
    FNameColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_NonDefaultValueColor(const Value: TColor);
begin
  if Value <> FNonDefaultValueColor then begin
    FNonDefaultValueColor := Value;
    Invalidate;
  end;
end;

function TzCustomObjectInspector.get_PlusMinBtnRect(Index: Integer): TRect;
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

function TzCustomObjectInspector.get_PropTextRect(Index: Integer): TRect;
begin
  Result := ItemRect[Index];
  Result.Left := (ItemOrder(VisibleItems[Index]) * FGutterWidth) + FGutterWidth + FSepTxtDis;
  Result.Right := SplitterPos;
end;

procedure TzCustomObjectInspector.set_ReadOnlyColor(const Value: TColor);
begin
  if FReadOnlyColor <> Value then begin
    FReadOnlyColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_ReferencesColor(const Value: TColor);
begin
  if Value <> FReferencesColor then begin
    FReferencesColor := Value;
    Invalidate;
  end;
end;

function TzCustomObjectInspector.GetSelectedItem: PzPropertyItem;
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

procedure TzCustomObjectInspector.set_ShowGridLines(const Value: Boolean);
begin
  if Value <> FShowGridLines then begin
    FShowGridLines := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_ShowGutter(const Value: Boolean);
begin
  if Value <> FShowGutter then begin
    FShowGutter := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_SubPropertiesColor(const Value: TColor);
begin
  if Value <> FSubPropertiesColor then begin
    FSubPropertiesColor := Value;
    Invalidate;
  end;
end;

procedure TzCustomObjectInspector.set_ValueColor(const Value: TColor);
begin
  if Value <> FValueColor then begin
    FValueColor := Value;
    Invalidate;
  end;
end;

function TzCustomObjectInspector.get_ValueRect(Index: Integer): TRect;
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

function TzCustomObjectInspector.get_ValueTextRect(Index: Integer): TRect;
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

{ TzObjectInspector }

class constructor TzObjectInspector.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TzObjectInspector, TzObjectInspectorScrollingStyleHook);
end;

class destructor TzObjectInspector.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TzObjectInspector, TzObjectInspectorScrollingStyleHook);
end;

procedure TzObjectInspectorDialogBase.DoCreate;
begin
  inherited;
end;

procedure TzObjectInspectorDialogBase.SetPropertyItem(const AItem: PzPropertyItem);
begin
  if FPropertyItem <> AItem then begin
    FPropertyItem := AItem;
    Setup;
  end;
end;

end.
