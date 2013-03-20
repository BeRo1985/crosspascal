unit PointerList;
{$I Compiler.inc}

interface

const MaxListSize=2147483647 div sizeof(pointer);

type PPointerArray=^TPointerArray;
     TPointerArray=array[0..MaxListSize-1] of pointer;

     TPointerList=class
      private
       FList:PPointerArray;
       FCount,FSize:longint;
       function GetItem(index:longint):pointer;
       procedure SetItem(index:longint;Value:pointer);
       function GetItemPointer(index:longint):pointer;
      public
       constructor Create;  
       destructor Destroy; override;
       procedure Clear;
       function Add(Item:pointer):longint;
       procedure Insert(index:longint;Item:pointer);
       procedure Delete(index:longint);
       function Remove(Item:pointer):longint;
       function Find(Item:pointer):longint;
       function IndexOf(Item:pointer):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       property Count:longint read FCount;
       property Capacity:longint read FSize write SetCapacity;
       property Item[index:longint]:pointer read GetItem write SetItem; default;
       property Items[index:longint]:pointer read GetItem write SetItem;
       property PItems[index:longint]:pointer read GetItemPointer;
     end;

implementation

constructor TPointerList.Create;
begin
 inherited Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 Clear;
end;

destructor TPointerList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TPointerList.Clear;
begin
 FCount:=0;
 FSize:=0;
 REALLOCMEM(FList,0);
end;

procedure TPointerList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  REALLOCMEM(FList,NewCapacity*sizeof(pointer));
  FSize:=NewCapacity;
 end;
end;

procedure TPointerList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FILLCHAR(FList^[FCount],(NewCount-FCount)*sizeof(pointer),#0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

function TPointerList.Add(Item:pointer):longint;
begin
 if FCount=FSize then begin
  if FSize>64 then begin
   inc(FSize,FSize div 4);
  end else if FSize>8 then begin
   inc(FSize,16);
  end else begin
   inc(FSize,4);
  end;
  REALLOCMEM(FList,FSize*sizeof(pointer));
 end;
 FList^[FCount]:=Item;
 result:=FCount;
 inc(FCount);
end;

procedure TPointerList.Insert(index:longint;Item:pointer);
var I:longint;
begin
 if (index>=0) and (index<FCount) then begin
  SetCount(FCount+1);
  for I:=FCount-1 downto index do FList^[I+1]:=FList^[I];
  FList^[index]:=Item;
 end else if index=FCount then begin
  Add(Item);
 end else if index>FCount then begin
  SetCount(index);
  Add(Item);
 end;
end;

procedure TPointerList.Delete(index:longint);
var I,J,K:longint;
begin
 if (index>=0) and (index<FCount) then begin
  K:=FCount-1;
  J:=index;
  for I:=J to K-1 do FList^[I]:=FList^[I+1];
  SetCount(K);
 end;
end;

function TPointerList.Remove(Item:pointer):longint;
var I,J,K:longint;
begin
 result:=-1;
 K:=FCount;
 J:=-1;
 for I:=0 to K-1 do begin
  if FList^[I]=Item then begin
   J:=I;
   break;
  end;
 end;
 if J>=0 then begin
  dec(K);
  for I:=J to K-1 do FList^[I]:=FList^[I+1];
  SetCount(K);
  result:=J;
 end;
end;

function TPointerList.Find(Item:pointer):longint;
var I:longint;
begin
 result:=-1;
 for I:=0 to FCount-1 do begin
  if FList^[I]=Item then begin
   result:=I;
   exit;
  end;
 end;
end;

function TPointerList.IndexOf(Item:pointer):longint;
var I:longint;
begin
 result:=-1;
 for I:=0 to FCount-1 do begin
  if FList^[I]=Item then begin
   result:=I;
   exit;
  end;
 end;
end;

procedure TPointerList.Exchange(Index1,Index2:longint);
var TempPointer:pointer;
begin
 if (Index1>=0) and (Index1<FCount) and (Index2>=0) and (Index2<FCount) then begin
  TempPointer:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=TempPointer;
 end;
end;

function TPointerList.GetItem(index:longint):pointer;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index];
 end else begin
  result:=nil;
 end;
end;

procedure TPointerList.SetItem(index:longint;Value:pointer);
begin
 if (index>=0) and (index<FCount) then FList^[index]:=Value;
end;

function TPointerList.GetItemPointer(index:longint):pointer;
begin
 if (index>=0) and (index<FCount) then begin
  result:=@FList^[index];
 end else begin
  result:=nil;
 end;
end;

end.
