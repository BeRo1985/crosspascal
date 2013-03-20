unit List;
{$I Compiler.inc}

interface

const MaxListSize=2147483647 div sizeof(pointer);

type PPointers=^TPointers;
     TPointers=array[0..MaxListSize-1] of pointer;

     TList=class
      private
       FList:PPointers;
       FCount,FSize:longint;
       function GetItem(index:longint):pointer;
       procedure SetItem(index:longint;Value:pointer);
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
     end;

implementation

constructor TList.Create;
begin
 inherited Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 Clear;
end;

destructor TList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TList.Clear;
begin
 FCount:=0;
 FSize:=0;
 REALLOCMEM(FList,0);
end;

procedure TList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  REALLOCMEM(FList,NewCapacity*sizeof(pointer));
  FSize:=NewCapacity;
 end;
end;

procedure TList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FILLCHAR(FList^[FCount],(NewCount-FCount)*sizeof(pointer),0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

function TList.Add(Item:pointer):longint;
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

procedure TList.Insert(index:longint;Item:pointer);
var I:longint;
begin
 if (index>=0) and (index<FCount) then begin
  SetCount(FCount+1);
  I:=FCount-1;
  while I>index do begin
   FList^[I]:=FList^[I-1];
   inc(I);
  end;
  FList^[index]:=Item;
 end else if index=FCount then begin
  Add(Item);
 end else if index>FCount then begin
  SetCount(index);
  Add(Item);
 end;
end;

procedure TList.Delete(index:longint);
var I,J,K:longint;
begin
 if (index>=0) and (index<FCount) then begin
  K:=FCount-1;
  J:=index;
  I:=J;
  while I<K do begin
   FList^[I]:=FList^[I+1];
   inc(I);
  end;
  SetCount(K);
 end;
end;

function TList.Remove(Item:pointer):longint;
var I,J,K:longint;
begin
 result:=-1;
 K:=FCount;
 J:=-1;
 I:=0;
 while I<K do begin
  if FList^[I]=Item then begin
   J:=I;
   break;
  end;
  inc(I);
 end;
 if J>=0 then begin
  dec(K);
  I:=J;
  while I<K do begin
   FList^[I]:=FList^[I+1];
   inc(I);
  end;
  SetCount(K);
  result:=J;
 end;
end;

function TList.Find(Item:pointer):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if FList^[I]=Item then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

function TList.IndexOf(Item:pointer):longint;
var I:longint;
begin
 result:=-1;
 I:=0;
 while I<FCount do begin
  if FList^[I]=Item then begin
   result:=I;
   exit;
  end;
  inc(I);
 end;
end;

procedure TList.Exchange(Index1,Index2:longint);
var TempPointer:pointer;
begin
 if (Index1>=0) and (Index1<FCount) and (Index2>=0) and (Index2<FCount) then begin
  TempPointer:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=TempPointer;
 end;
end;

function TList.GetItem(index:longint):pointer;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index];
 end else begin
  result:=nil;
 end;
end;

procedure TList.SetItem(index:longint;Value:pointer);
begin
 if (index>=0) and (index<FCount) then FList^[index]:=Value;
end;

end.
