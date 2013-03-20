unit IntegerList;
{$I Compiler.inc}

interface

const MaxListSize=2147483647 div sizeof(integer);

type PIntegerArray=^TIntegerArray;
     TIntegerArray=array[0..MaxListSize-1] of integer;

     TIntegerList=class
      private
       FList:PIntegerArray;
       FCount,FSize:longint;
       function GetItem(index:longint):longint;
       procedure SetItem(index:longint;Value:longint);
       function GetItemPointer(index:longint):pointer;
      public
       constructor Create;  
       destructor Destroy; override;
       procedure Clear;
       function Add(Item:longint):longint;
       procedure Insert(index:longint;Item:longint);
       procedure Delete(index:longint);
       function Remove(Item:longint):longint;
       function Find(Item:longint):longint;
       function IndexOf(Item:longint):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       property Count:longint read FCount;
       property Capacity:longint read FSize write SetCapacity;
       property Item[index:longint]:longint read GetItem write SetItem; default;
       property Items[index:longint]:longint read GetItem write SetItem;
       property PItems[index:longint]:pointer read GetItemPointer;
     end;

implementation

constructor TIntegerList.Create;
begin
 inherited Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 Clear;
end;

destructor TIntegerList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TIntegerList.Clear;
begin
 FCount:=0;
 FSize:=0;
 REALLOCMEM(FList,0);
end;

procedure TIntegerList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  REALLOCMEM(FList,NewCapacity*sizeof(integer));
  FSize:=NewCapacity;
 end;
end;

procedure TIntegerList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FILLCHAR(FList^[FCount],(NewCount-FCount)*sizeof(integer),#0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

function TIntegerList.Add(Item:longint):longint;
begin
 if FCount=FSize then begin
  if FSize>64 then begin
   inc(FSize,FSize div 4);
  end else if FSize>8 then begin
   inc(FSize,16);
  end else begin
   inc(FSize,4);
  end;
  REALLOCMEM(FList,FSize*sizeof(integer));
 end;
 FList^[FCount]:=Item;
 result:=FCount;
 inc(FCount);
end;

procedure TIntegerList.Insert(index:longint;Item:longint);
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

procedure TIntegerList.Delete(index:longint);
var I,J,K:longint;
begin
 if (index>=0) and (index<FCount) then begin
  K:=FCount-1;
  J:=index;
  for I:=J to K-1 do FList^[I]:=FList^[I+1];
  SetCount(K);
 end;
end;

function TIntegerList.Remove(Item:longint):longint;
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

function TIntegerList.Find(Item:longint):longint;
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

function TIntegerList.IndexOf(Item:longint):longint;
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

procedure TIntegerList.Exchange(Index1,Index2:longint);
var TempInteger:longint;
begin
 if (Index1>=0) and (Index1<FCount) and (Index2>=0) and (Index2<FCount) then begin
  TempInteger:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=TempInteger;
 end;
end;

function TIntegerList.GetItem(index:longint):longint;
begin
 if (index>=0) and (index<FCount) then begin
  result:=FList^[index];
 end else begin
  result:=0;
 end;
end;

procedure TIntegerList.SetItem(index:longint;Value:longint);
begin
 if (index>=0) and (index<FCount) then FList^[index]:=Value;
end;

function TIntegerList.GetItemPointer(index:longint):pointer;
begin
 if (index>=0) and (index<FCount) then begin
  result:=@FList^[index];
 end else begin
  result:=nil;
 end;
end;

end.
