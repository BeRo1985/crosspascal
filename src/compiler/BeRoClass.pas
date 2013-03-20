unit BeRoClass;
{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

interface

type TBeRoClass=class
      public
       Previous,Next:TBeRoClass;
       BeRoCore:pointer;
       constructor Create; overload; virtual; 
       destructor Destroy; override;
     end;

const MaxListSize=2147483647 div sizeof(TBeRoClass);

type PEngineListClasses=^TBeRoClasses;
     TBeRoClasses=array[0..MaxListSize-1] of TBeRoClass;

     TBeRoClassList=class(TBeRoClass)
      private
       InternalList:PEngineListClasses;
       InternalCount,InternalCapacity:integer;
       function GetItem(index:integer):TBeRoClass;
       procedure SetItem(index:integer;Value:TBeRoClass);
       function GetItemPointer(index:integer):TBeRoClass;
      public
       ClearWithContentDestroying:boolean;
       CapacityMask:integer;
       CapacityMinimium:integer;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       function Add(Item:TBeRoClass):integer;
       function Append(Item:TBeRoClass):integer;
       function AddList(List:TBeRoClassList):integer;
       function AppendList(List:TBeRoClassList):integer;
       function NewClass:TBeRoClass;
       procedure Insert(index:integer;Item:TBeRoClass);
       procedure Delete(index:integer);
       procedure DeleteClass(index:integer);
       function Remove(Item:TBeRoClass):integer;
       function RemoveClass(Item:TBeRoClass):integer;
       function Find(Item:TBeRoClass):integer;
       function IndexOf(Item:TBeRoClass):integer;
       procedure Exchange(Index1,Index2:integer);
       procedure SetCapacity(NewCapacity:integer);
       procedure SetOptimalCapacity(TargetCapacity:integer);
       procedure SetCount(NewCount:integer);
       function Push(Item:TBeRoClass):integer;
       function Pop(var Item:TBeRoClass):boolean; overload;
       function Pop:TBeRoClass; overload;
       function Last:TBeRoClass;
       property Count:integer read InternalCount; 
       property Capacity:integer read InternalCapacity write SetCapacity;
       property Item[index:integer]:TBeRoClass read GetItem write SetItem; default;
       property Items[index:integer]:TBeRoClass read GetItem write SetItem;
       property PItems[index:integer]:TBeRoClass read GetItemPointer;
     end;

     TBeRoClassLinkedList=class(TBeRoClass)
      public
       ClearWithContentDestroying:boolean;
       First,Last:TBeRoClass;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       procedure Add(Item:TBeRoClass);
       procedure Append(Item:TBeRoClass);
       procedure AddLinkedList(List:TBeRoClassLinkedList);
       procedure AppendLinkedList(List:TBeRoClassLinkedList);
       procedure Remove(Item:TBeRoClass);
       procedure RemoveClass(Item:TBeRoClass);
       procedure Push(Item:TBeRoClass);
       function Pop(var Item:TBeRoClass):boolean; overload;
       function Pop:TBeRoClass; overload;
       function Count:integer;
     end;

const CurrentBeRoCore:pointer=nil;

implementation

uses BeRoUtils;

constructor TBeRoClass.Create;
begin
 inherited Create;
 Previous:=nil;
 Next:=nil;
 BeRoCore:=CurrentBeRoCore;
end;

destructor TBeRoClass.Destroy;
begin
 inherited Destroy;
end;

constructor TBeRoClassList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 InternalCount:=0;
 InternalCapacity:=0;
 InternalList:=nil;
 CapacityMinimium:=0;
 {$IFDEF CPU32}
  CapacityMask:=($1000 shr 2)-1;
 {$ELSE}
  CapacityMask:=($1000 shr 3)-1;
 {$ENDIF}
 Clear;
end;

destructor TBeRoClassList.Destroy;
begin
 Clear;
 if assigned(InternalList) and (InternalCapacity<>0) then begin
  FREEMEM(InternalList);
 end;
 inherited Destroy;
end;

procedure TBeRoClassList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TBeRoClassList.ClearNoFree;
begin
 SetCount(0);
end;

procedure TBeRoClassList.ClearWithFree;
var Counter:integer;
begin
 for Counter:=0 to InternalCount-1 do begin
  if assigned(InternalList^[Counter]) then begin
   try
    InternalList^[Counter].Destroy;
   except
   end;
  end;
 end;
 SetCount(0);
end;

procedure TBeRoClassList.SetCapacity(NewCapacity:integer);
begin
 if (InternalCapacity<>NewCapacity) and
    ((NewCapacity>=0) and (NewCapacity<MaxListSize)) then begin
  REALLOCMEM(InternalList,NewCapacity*sizeof(TBeRoClass));
  if InternalCapacity<NewCapacity then begin
   FastFillChar(InternalList^[InternalCapacity],(NewCapacity-InternalCapacity)*sizeof(TBeRoClass),#0);
  end;
  InternalCapacity:=NewCapacity;
 end;
end;

procedure TBeRoClassList.SetOptimalCapacity(TargetCapacity:integer);
begin
 if (TargetCapacity>=0) and (TargetCapacity<MaxListSize) then begin
  SetCapacity((TargetCapacity+CapacityMask+CapacityMinimium) and not CapacityMask);
 end;
end;

procedure TBeRoClassList.SetCount(NewCount:integer);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  SetOptimalCapacity(NewCount);
  if InternalCount<NewCount then begin
   FastFillChar(InternalList^[InternalCount],(NewCount-InternalCount)*sizeof(TBeRoClass),#0);
  end;
  InternalCount:=NewCount;
 end;
end;

function TBeRoClassList.Add(Item:TBeRoClass):integer;
begin
 result:=InternalCount;
 SetCount(result+1);
 InternalList^[result]:=Item;
end;

function TBeRoClassList.Append(Item:TBeRoClass):integer;
begin
 result:=Add(Item);
end;

function TBeRoClassList.AddList(List:TBeRoClassList):integer;
var Counter,index:integer;
begin
 result:=-1;
 for Counter:=0 to List.Count-1 do begin
  index:=Add(List[Counter]);
  if Counter=0 then begin
   result:=index;
  end;
 end;
end;

function TBeRoClassList.AppendList(List:TBeRoClassList):integer;
begin
 result:=AddList(List);
end;

function TBeRoClassList.NewClass:TBeRoClass;
var Item:TBeRoClass;
begin
 Item:=TBeRoClass.Create;
 Add(Item);
 result:=Item;
end;

procedure TBeRoClassList.Insert(index:integer;Item:TBeRoClass);
var Counter:integer;
begin
 if (index>=0) and (index<InternalCount) then begin
  SetCount(InternalCount+1);
  for Counter:=InternalCount-1 downto index do begin
   InternalList^[Counter+1]:=InternalList^[Counter];
  end;
  InternalList^[index]:=Item;
 end else if index=InternalCount then begin
  Add(Item);
 end else if index>InternalCount then begin
  SetCount(index);
  Add(Item);
 end;
end;

procedure TBeRoClassList.Delete(index:integer);
var I,J:integer;
begin
 if (index>=0) and (index<InternalCount) then begin
  J:=InternalCount-1;
  I:=index;
  MOVE(InternalList^[I+1],InternalList^[I],(J-I)*sizeof(TBeRoClass));
  SetCount(J);
 end;
end;

procedure TBeRoClassList.DeleteClass(index:integer);
var I,J:integer;
begin
 if (index>=0) and (index<InternalCount) then begin
  J:=InternalCount-1;
  I:=index;
  if assigned(InternalList^[I]) then begin
   try
    InternalList^[I].Destroy;
   except
   end;
   InternalList^[I]:=nil;
  end;
  MOVE(InternalList^[I+1],InternalList^[I],(J-I)*sizeof(TBeRoClass));
  SetCount(J);
 end;
end;

function TBeRoClassList.Remove(Item:TBeRoClass):integer;
var I,J,K:integer;
begin
 result:=-1;
 K:=InternalCount;
 J:=-1;
 for I:=0 to K-1 do begin
  if InternalList^[I]=Item then begin
   J:=I;
   break;
  end;
 end;
 if J>=0 then begin
  dec(K);
  MOVE(InternalList^[J+1],InternalList^[J],(K-J)*sizeof(TBeRoClass));
  SetCount(K);
  result:=J;
 end;
end;

function TBeRoClassList.RemoveClass(Item:TBeRoClass):integer;
var I,J,K:integer;
begin
 result:=-1;
 K:=InternalCount;
 J:=-1;
 for I:=0 to K-1 do begin
  if InternalList^[I]=Item then begin
   J:=I;
   break;
  end;
 end;
 if J>=0 then begin
  dec(K);
  MOVE(InternalList^[J+1],InternalList^[J],(K-J)*sizeof(TBeRoClass));
  SetCount(K);
  if assigned(Item) then begin
   try
    Item.Destroy;
   except
   end;
  end;
  result:=J;
 end;
end;

function TBeRoClassList.Find(Item:TBeRoClass):integer;
var I:integer;
begin
 result:=-1;
 for I:=0 to InternalCount-1 do begin
  if InternalList^[I]=Item then begin
   result:=I;
   exit;
  end;
 end;
end;

function TBeRoClassList.IndexOf(Item:TBeRoClass):integer;
var I:integer;
begin
 result:=-1;
 for I:=0 to InternalCount-1 do begin
  if InternalList^[I]=Item then begin
   result:=I;
   exit;
  end;
 end;
end;

procedure TBeRoClassList.Exchange(Index1,Index2:integer);
var TempPointer:TBeRoClass;
begin
 if (Index1>=0) and (Index1<InternalCount) and (Index2>=0) and (Index2<InternalCount) then begin
  TempPointer:=InternalList^[Index1];
  InternalList^[Index1]:=InternalList^[Index2];
  InternalList^[Index2]:=TempPointer;
 end;
end;

function TBeRoClassList.Push(Item:TBeRoClass):integer;
begin
 result:=Add(Item);
end;

function TBeRoClassList.Pop(var Item:TBeRoClass):boolean;
begin
 result:=InternalCount>0;
 if result then begin
  Item:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end;
end;

function TBeRoClassList.Pop:TBeRoClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end else begin
  result:=nil;
 end;
end;

function TBeRoClassList.Last:TBeRoClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
 end else begin
  result:=nil;
 end;
end;

function TBeRoClassList.GetItem(index:integer):TBeRoClass;
begin
 if (index>=0) and (index<InternalCount) then begin
  result:=InternalList^[index];
 end else begin
  result:=nil;
 end;
end;

procedure TBeRoClassList.SetItem(index:integer;Value:TBeRoClass);
begin
 if (index>=0) and (index<InternalCount) then begin
  InternalList^[index]:=Value;
 end;
end;

function TBeRoClassList.GetItemPointer(index:integer):TBeRoClass;
begin
 if (index>=0) and (index<InternalCount) then begin
  result:=@InternalList^[index];
 end else begin
  result:=nil;
 end;
end;

constructor TBeRoClassLinkedList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 ClearNoFree;
end;

destructor TBeRoClassLinkedList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoClassLinkedList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TBeRoClassLinkedList.ClearNoFree;
var Current,Next:TBeRoClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  Remove(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TBeRoClassLinkedList.ClearWithFree;
var Current,Next:TBeRoClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  RemoveClass(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TBeRoClassLinkedList.Add(Item:TBeRoClass);
begin
 Item.Next:=nil;
 if assigned(Last) then begin
  Last.Next:=Item;
  Item.Previous:=Last;
 end else begin
  Item.Previous:=nil;
  First:=Item;
 end;
 Last:=Item;
end;

procedure TBeRoClassLinkedList.Append(Item:TBeRoClass);
begin
 Add(Item);
end;

procedure TBeRoClassLinkedList.AddLinkedList(List:TBeRoClassLinkedList);
begin
 Last.Next:=List.First;
 if assigned(List.First) then begin
  List.First.Previous:=Last;
 end;
 Last:=List.Last;
 List.First:=nil;
 List.Last:=nil;
end;

procedure TBeRoClassLinkedList.AppendLinkedList(List:TBeRoClassLinkedList);
begin
 AddLinkedList(List);
end;

procedure TBeRoClassLinkedList.Remove(Item:TBeRoClass);
begin
 if not assigned(Item) then exit;
 if First=Item then First:=Item.Next;
 if Last=Item then Last:=Item.Previous;
 if assigned(Item.Next) then Item.Next.Previous:=Item.Previous;
 if assigned(Item.Previous) then Item.Previous.Next:=Item.Next;
 Item.Previous:=nil;
 Item.Next:=nil;
end;

procedure TBeRoClassLinkedList.RemoveClass(Item:TBeRoClass);
begin
 if not assigned(Item) then exit;
 Remove(Item);
 Item.Destroy;
end;

procedure TBeRoClassLinkedList.Push(Item:TBeRoClass);
begin
 Add(Item);
end;

function TBeRoClassLinkedList.Pop(var Item:TBeRoClass):boolean;
begin
 result:=assigned(Last);
 if result then begin
  Item:=Last;
  Remove(Last);
 end;
end;

function TBeRoClassLinkedList.Pop:TBeRoClass;
begin
 result:=Last;
 if assigned(Last) then Remove(Last);
end;

function TBeRoClassLinkedList.Count:integer;
var Current:TBeRoClass;
begin
 result:=0;
 Current:=First;
 while assigned(Current) do begin
  inc(result);
  Current:=Current.Next;
 end;
end;

end.
