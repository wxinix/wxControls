unit wxObjectInspector.Exceptions;

interface

uses
  System.SysUtils;

type
  EObjectInspectorException = class(Exception);

  EIndexOutOfRange = class(EObjectInspectorException)
  public
    constructor Create;
  end;

  EInvalidDialogClass = class(EObjectInspectorException)
  public
    constructor Create;
  end;

  EInvalidGutterWidth = class(EObjectInspectorException)
  public
    constructor Create;
  end;


  EInvalidListBoxClass = class(EObjectInspectorException)
  public
    constructor Create;
  end;

  EInvalidObjectVisibility = class(EObjectInspectorException)
  public
    constructor Create;
  end;

  EInvalidPropertyValue = class(EObjectInspectorException)
  public
    constructor Create(const AItemName, AValue: string);
  end;

  EItemOwnerListNotAssigned = class(EObjectInspectorException)
  public
    constructor Create;
  end;

  EItemHasNoListBox = class(EObjectInspectorException)
  public
    constructor Create(const AItemName: string);
  end;

implementation

uses
  wxObjectInspector.ValueManager;

resourcestring
  SIndexOutOfRange = 'The specified item index is out of range for the current inspector.';
  SInvalidDialogClass = 'Dialog must be derived from TCommonDialog or TzObjectInspectorDialog';
  SInvalidGutterWidth = 'Gutter width must be less than %d';
  SInvalidListBoxClass = 'List box class must be derived from TzObjectInspectorListBox';
  SInvalidObjectVisibility = 'Object visibilty must be Public or Published';
  SInvalidPropertyValue = 'The specified value ''%s'' is invalid for property item ''%s''.';
  SItemHasNoListBox = 'Item ''%s'' has no list box associated with it';
  SItemOwnerListNotAssigned = 'Property item''s owner list not assigned.';
  SNonVisibleItemSelected = 'Could not select a non-visible property item from the inspector.';

constructor EInvalidDialogClass.Create;
begin
  inherited CreateRes(@SInvalidDialogClass);
end;

constructor EIndexOutOfRange.Create;
begin
  inherited CreateRes(@SIndexOutOfRange);
end;

constructor EInvalidListBoxClass.Create;
begin
  inherited CreateRes(@SInvalidListBoxClass);
end;

constructor EInvalidPropertyValue.Create(const AItemName, AValue: string);
begin
  inherited CreateResFmt(@SInvalidPropertyValue, [AValue, AItemName]);
end;

constructor EItemOwnerListNotAssigned.Create;
begin
  inherited CreateRes(@SItemOwnerListNotAssigned);
end;

constructor EItemHasNoListBox.Create(const AItemName: string);
begin
  inherited CreateResFmt(@SItemHasNoListBox, [AItemName]);
end;

constructor EInvalidObjectVisibility.Create;
begin
  inherited CreateRes(@SInvalidObjectVisibility);
end;

constructor EInvalidGutterWidth.Create;
begin
  inherited CreateResFmt(@SInvalidGutterWidth, [ValueManager.MaximumGutterWidth]);
end;

end.
