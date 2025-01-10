# Search all string columns in all SQL Server databases

https://www.mssqltips.com/sqlservertip/4039/search-all-string-columns-in-all-sql-server-databases/

https://www.mssqltips.com/wp-content/images-tips/4039_ABSearchAllDatabases.txt

# SP: SearchAllDatabases

```
CREATE PROCEDURE dbo.SearchAllDatabases
  @SearchTerm NVARCHAR(255) = NULL
AS
BEGIN
  SET NOCOUNT ON;
  SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
  
  IF @SearchTerm IS NULL OR @SearchTerm NOT LIKE N'%[^%^_]%'
  BEGIN
    RAISERROR(N'Please enter a valid search term.', 11, 1);
    RETURN;
  END
 
  CREATE TABLE #results
  (
    [database]   SYSNAME,
    [schema]     SYSNAME,
    [table]      SYSNAME, 
    [column]     SYSNAME,
    ExampleValue NVARCHAR(1000)
  );

  DECLARE
    @DatabaseCommands  NVARCHAR(MAX) = N'', 
    @ColumnCommands    NVARCHAR(MAX) = N'';

  SELECT @DatabaseCommands = @DatabaseCommands + N'
    EXEC ' + QUOTENAME(name) + '.sys.sp_executesql 
        @ColumnCommands, N''@SearchTerm NVARCHAR(MAX)'', @SearchTerm;'
    FROM sys.databases 
    WHERE database_id  > 4  -- non-system databases  
      AND [state]      = 0  -- online 
      AND user_access  = 0; -- multi-user

    SET @ColumnCommands = N'DECLARE @q NCHAR(1),
          @SearchCommands NVARCHAR(MAX);
    
    SELECT @q = NCHAR(39), 
      @SearchCommands = N''DECLARE @VSearchTerm VARCHAR(255) = @SearchTerm;'';
  
    SELECT @SearchCommands = @SearchCommands + CHAR(10) + N''

      SELECT TOP (1)
        [db]     = DB_NAME(),
        [schema] = N'' + @q + s.name + @q + '', 
        [table]  = N'' + @q + t.name + @q + '',
        [column] = N'' + @q + c.name + @q + '',
        ExampleValue = LEFT('' + QUOTENAME(c.name) + '', 1000) 
      FROM '' + QUOTENAME(s.name) + ''.'' + QUOTENAME(t.name) + ''
      WHERE '' + QUOTENAME(c.name) + N'' LIKE @'' + CASE 
        WHEN c.system_type_id IN (35, 167, 175) THEN ''V'' 
        ELSE '''' END + ''SearchTerm;'' 
 
    FROM sys.schemas AS s
    INNER JOIN sys.tables AS t
    ON s.[schema_id] = t.[schema_id]
    INNER JOIN sys.columns AS c
    ON t.[object_id] = c.[object_id]
    WHERE c.system_type_id IN (35, 99, 167, 175, 231, 239)
      AND c.max_length >= LEN(@SearchTerm);

    PRINT @SearchCommands;
    EXEC sys.sp_executesql @SearchCommands, 
      N''@SearchTerm NVARCHAR(255)'', @SearchTerm;';
  
  INSERT #Results
  (
    [database],
    [schema],
    [table],
    [column],
    ExampleValue
  )
  EXEC [master].sys.sp_executesql @DatabaseCommands, 
    N'@ColumnCommands NVARCHAR(MAX), @SearchTerm NVARCHAR(255)', 
    @ColumnCommands, @SearchTerm;

  SELECT [Searched for] = @SearchTerm;
  
  SELECT [database],[schema],[table],[column],ExampleValue 
    FROM #Results 
    ORDER BY [database],[schema],[table],[column];
END
GO

-- generate an error because this is not a sensible search:
EXEC dbo.SearchAllDatabases @SearchTerm = N'%';
GO

-- search for columns containing "http://"
EXEC dbo.SearchAllDatabases @SearchTerm = N'%http://%';

-- search for columns containing "%" or "_":
EXEC dbo.SearchAllDatabases @SearchTerm = N'%[%]%';
EXEC dbo.SearchAllDatabases @SearchTerm = N'%[_]%';

-- search for columns containing "update" followed by "dbo.":
EXEC dbo.SearchAllDatabases @SearchTerm = N'%update%dbo.%';
```

# Search and Find String Value in all SQL Server Table Columns

https://www.mssqltips.com/sqlservertip/1522/searching-and-finding-a-string-value-in-all-columns-in-a-sql-server-table/

## Option 1 – Uses a Cursor (original method)
```
USE master 
GO 

CREATE PROCEDURE dbo.sp_FindStringInTable @stringToFind VARCHAR(100), @schema sysname, @table sysname 
AS 
SET NOCOUNT ON
DECLARE @sqlCommand VARCHAR(8000) 
DECLARE @where VARCHAR(8000) 
DECLARE @columnName sysname 
DECLARE @cursor VARCHAR(8000) 
BEGIN TRY 
   SET @sqlCommand = 'SELECT * FROM [' + @schema + '].[' + @table + '] WHERE' 
   SET @where = '' 
   SET @cursor = 'DECLARE col_cursor CURSOR FOR SELECT COLUMN_NAME 
   FROM ' + DB_NAME() + '.INFORMATION_SCHEMA.COLUMNS 
   WHERE TABLE_SCHEMA = ''' + @schema + ''' 
   AND TABLE_NAME = ''' + @table + ''' 
   AND DATA_TYPE IN (''char'',''nchar'',''ntext'',''nvarchar'',''text'',''varchar'')' 
   EXEC (@cursor) 
   OPEN col_cursor    
   FETCH NEXT FROM col_cursor INTO @columnName    
   WHILE @@FETCH_STATUS = 0    
   BEGIN    
       IF @where <> '' 
           SET @where = @where + ' OR' 
       SET @where = @where + ' [' + @columnName + '] LIKE ''' + @stringToFind + '''' 
       FETCH NEXT FROM col_cursor INTO @columnName    
   END    
   CLOSE col_cursor    
   DEALLOCATE col_cursor  
   SET @sqlCommand = @sqlCommand + @where 
   PRINT @sqlCommand 
   EXEC (@sqlCommand)  
END TRY 
BEGIN CATCH 
   PRINT 'There was an error. Check to make sure object exists.'
   PRINT error_message()
    
   IF CURSOR_STATUS('variable', 'col_cursor') <> -3 
   BEGIN 
       CLOSE col_cursor    
       DEALLOCATE col_cursor  
   END 
END CATCH 
```

## Option 2 – Does Not Use a Cursor (new method)
```

SELECT is_local_cursor_default FROM sys.databases WHERE name = DB_NAME()

USE master
GO

CREATE PROCEDURE dbo.sp_FindStringInTable @stringToFind VARCHAR(max), @schema sysname, @table sysname 
AS
SET NOCOUNT ON
BEGIN TRY
   DECLARE @sqlCommand varchar(max) = 'SELECT * FROM [' + @schema + '].[' + @table + '] WHERE ' 
   
   SELECT @sqlCommand = @sqlCommand + '[' + COLUMN_NAME + '] LIKE ''' + @stringToFind + ''' OR '
   FROM INFORMATION_SCHEMA.COLUMNS 
   WHERE TABLE_SCHEMA = @schema
   AND TABLE_NAME = @table 
   AND DATA_TYPE IN ('char','nchar','ntext','nvarchar','text','varchar')
   SET @sqlCommand = left(@sqlCommand,len(@sqlCommand)-3)
   EXEC (@sqlCommand)
   PRINT @sqlCommand
END TRY
BEGIN CATCH 
   PRINT 'There was an error. Check to make sure object exists.'
   PRINT error_message()
END CATCH 
```

## Usage
```
USE AdventureWorks 
GO

EXEC sp_FindStringInTable 'Irv%', 'Person', 'Address'
GO
```

## Option 3: Show Columns that Match
```
CREATE PROCEDURE dbo.sp_FindStringInTable_with_flag @stringToFind VARCHAR(max), @schema sysname, @table sysname 
AS
SET NOCOUNT ON
BEGIN TRY
   DECLARE @sqlCommand varchar(max) = 'SELECT ' 
   SELECT @sqlCommand = @sqlCommand + 'case when [' + COLUMN_NAME + '] LIKE ''' + @stringToFind + ''' then 1 else 0 end as ' + COLUMN_NAME + '_found, ' 
   FROM INFORMATION_SCHEMA.COLUMNS 
   WHERE TABLE_SCHEMA = @schema
   AND TABLE_NAME = @table 
   AND DATA_TYPE IN ('char','nchar','ntext','nvarchar','text','varchar')
   SELECT @sqlCommand = @sqlCommand + ' * FROM [' + @schema + '].[' + @table + '] WHERE '
   
   SELECT @sqlCommand = @sqlCommand + '[' + COLUMN_NAME + '] LIKE ''' + @stringToFind + ''' OR '
   FROM INFORMATION_SCHEMA.COLUMNS 
   WHERE TABLE_SCHEMA = @schema
   AND TABLE_NAME = @table 
   AND DATA_TYPE IN ('char','nchar','ntext','nvarchar','text','varchar')
   SET @sqlCommand = left(@sqlCommand,len(@sqlCommand)-3)
   EXEC (@sqlCommand)
   PRINT @sqlCommand
END TRY
BEGIN CATCH 
   PRINT 'There was an error. Check to make sure object exists.'
   PRINT error_message()
END CATCH 
```
## Usage
```
USE master
GO

EXEC sys.sp_MS_marksystemobject sp_FindStringInTable_with_flag
GO
```

# SP: SearchAllTables

```
CREATE PROC SearchAllTables
(
	@SearchStr nvarchar(100)
)
AS
BEGIN

	-- Copyright © 2002 Narayana Vyas Kondreddi. All rights reserved.
	-- Purpose: To search all columns of all tables for a given search string
	-- Written by: Narayana Vyas Kondreddi
	-- Site: http://vyaskn.tripod.com
	-- Tested on: SQL Server 7.0 and SQL Server 2000
	-- Date modified: 28th July 2002 22:50 GMT


	CREATE TABLE #Results (ColumnName nvarchar(370), ColumnValue nvarchar(3630))

	SET NOCOUNT ON

	DECLARE @TableName nvarchar(256), @ColumnName nvarchar(128), @SearchStr2 nvarchar(110)
	SET  @TableName = ''
	SET @SearchStr2 = QUOTENAME('%' + @SearchStr + '%','''')

	WHILE @TableName IS NOT NULL
	BEGIN
		SET @ColumnName = ''
		SET @TableName = 
		(
			SELECT MIN(QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME))
			FROM 	INFORMATION_SCHEMA.TABLES
			WHERE 		TABLE_TYPE = 'BASE TABLE'
				AND	QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME) > @TableName
				AND	OBJECTPROPERTY(
						OBJECT_ID(
							QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME)
							 ), 'IsMSShipped'
						       ) = 0
		)

		WHILE (@TableName IS NOT NULL) AND (@ColumnName IS NOT NULL)
		BEGIN
			SET @ColumnName =
			(
				SELECT MIN(QUOTENAME(COLUMN_NAME))
				FROM 	INFORMATION_SCHEMA.COLUMNS
				WHERE 		TABLE_SCHEMA	= PARSENAME(@TableName, 2)
					AND	TABLE_NAME	= PARSENAME(@TableName, 1)
					AND	DATA_TYPE IN ('char', 'varchar', 'nchar', 'nvarchar')
					AND	QUOTENAME(COLUMN_NAME) > @ColumnName
			)
	
			IF @ColumnName IS NOT NULL
			BEGIN
				INSERT INTO #Results
				EXEC
				(
					'SELECT ''' + @TableName + '.' + @ColumnName + ''', LEFT(' + @ColumnName + ', 3630) 
					FROM ' + @TableName + ' (NOLOCK) ' +
					' WHERE ' + @ColumnName + ' LIKE ' + @SearchStr2
				)
			END
		END	
	END

	SELECT ColumnName, ColumnValue FROM #Results
END
```
