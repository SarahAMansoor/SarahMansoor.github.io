DROP SCHEMA IF EXISTS projectschema CASCADE;
CREATE SCHEMA projectschema;
SET SEARCH_PATH TO projectschema;


-- A school Board. 
-- BID is the school board's ID, bName is the school
-- board's name, bLanguage is the school board's
-- language, and bType is the school board's type
CREATE TABLE Board (
    BID TEXT PRIMARY KEY,
    bName TEXT NOT NULL,
    bLanguage TEXT NOT NULL,
    bType TEXT NOT NULL);

-- EQAO results and progress by school board
-- BID is the school board's ID, eResults 
-- is the school board's EQAO results, 
-- eProgress is the school board's EQAO progress
CREATE TABLE EQAO (
    BID TEXT PRIMARY KEY,
    eResults DECIMAL NOT NULL,
    eProgress DECIMAL NOT NULL,
    FOREIGN KEY(BID) REFERENCES Board(BID)
);

-- OSSLT results and progress by school board
-- BID is the school board's ID, oResults 
-- is the school board's OSSLT results, 
-- oProgress is the school board's OSSLT progress
CREATE TABLE OSSLT (
    BID TEXT PRIMARY KEY,
    oResults DECIMAL NOT NULL,
    oProgress DECIMAL NOT NULL,
    FOREIGN KEY (BID) REFERENCES Board(BID)
);


-- The credit accumulation results 
-- and progress by school board
-- BID is the school board's ID, 
-- credit is the school board's 
-- credit accumulation results,
-- and progress is the school board's 
-- credit accumulation progress
CREATE TABLE CreditAccumulation (
    BID TEXT PRIMARY KEY,
    credit DECIMAL NOT NULL,
    progress DECIMAL NOT NULL,
    FOREIGN KEY (BID) REFERENCES Board(BID)
);

-- The graduation results and progress
-- for four year and five year graduation
-- by school board
-- BID is the school board's ID, 
-- fourRate is the four year rate of graduation, 
-- fourProgress is the four year progress of graduation, 
-- fiveRate is the five year rate of graduation, 
-- and fiveYear is the five year progress of graduation
CREATE TABLE Graduation (
    BID TEXT PRIMARY KEY,
    fourRate DECIMAL NOT NULL,
    fourProgress DECIMAL NOT NULL,
    fiveRate DECIMAL NULL,
    fiveProgress DECIMAL NOT NULL,
    FOREIGN KEY (BID) REFERENCES Board(BID)
);





