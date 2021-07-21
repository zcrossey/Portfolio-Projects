/*
	Cleaning Data in SQL Queries
*/

SELECT *
FROM PortfolioProject.dbo.['Nashville Housing$']

------------------------------------------------------------------
-- Standardize Date Format

SELECT SaleDate, CONVERT(Date, SaleDate)
FROM PortfolioProject.dbo.['Nashville Housing$']

ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD SaleDateConverted Date;

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET SaleDateConverted = CONVERT(Date, SaleDate)

SELECT SaleDateConverted
FROM PortfolioProject.dbo.['Nashville Housing$']

-------------------------------------------------------------------

-- Populate Property Address Data

SELECT PropertyAddress
FROM PortfolioProject.dbo.['Nashville Housing$']
WHERE PropertyAddress IS NULL

SELECT *
FROM PortfolioProject.dbo.['Nashville Housing$']
--WHERE PropertyAddress IS NULL
ORDER BY ParcelID

SELECT nash1.ParcelID, nash1.PropertyAddress, nash2.ParcelID, nash2.PropertyAddress, ISNULL(nash1.PropertyAddress, nash2.PropertyAddress)
FROM PortfolioProject.dbo.['Nashville Housing$'] AS nash1
JOIN PortfolioProject.dbo.['Nashville Housing$'] AS nash2
	ON nash1.ParcelID = nash2.ParcelID
	AND nash1.[UniqueID ] <> nash2.[UniqueID ]
WHERE nash1.PropertyAddress	IS NULL


UPDATE nash1
SET	PropertyAddress = ISNULL(nash1.PropertyAddress, nash2.PropertyAddress)
FROM PortfolioProject.dbo.['Nashville Housing$'] AS nash1
JOIN PortfolioProject.dbo.['Nashville Housing$'] AS nash2
	ON nash1.ParcelID = nash2.ParcelID
	AND nash1.[UniqueID ] <> nash2.[UniqueID ]
WHERE nash1.PropertyAddress	IS NULL


----------------------------------------------------------------------------

-- Breaking Out Address Into Individual Columns (Address, City, State)

-- Method #1
SELECT PropertyAddress
FROM PortfolioProject.dbo.['Nashville Housing$']

SELECT
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1) AS Address,
SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress)) AS City
FROM PortfolioProject.dbo.['Nashville Housing$']


ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD PropertySplitAddress NVARCHAR(255);

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1)

ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD PropertySplitCity NVARCHAR(255);

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress))

SELECT *
FROM PortfolioProject.dbo.['Nashville Housing$']


-- Method #2
SELECT OwnerAddress
FROM PortfolioProject.dbo.['Nashville Housing$']

SELECT 
PARSENAME(REPLACE(OwnerAddress,',', '.'),3) AS Address,
PARSENAME(REPLACE(OwnerAddress,',', '.'),2) AS City,
PARSENAME(REPLACE(OwnerAddress,',', '.'),1) AS State
FROM PortfolioProject.dbo.['Nashville Housing$']


ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD OwnerSplitAddress NVARCHAR(255);

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress,',', '.'),3)

ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD OwnerSplitCity NVARCHAR(255);

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress,',', '.'),2) 

ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
ADD OwnerSplitState NVARCHAR(255);

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress,',', '.'),1) 

SELECT *
FROM PortfolioProject.dbo.['Nashville Housing$']

---------------------------------------------------------------------------

-- Change Y and N to Yes and No in "Sold as Vacant" Field

SELECT DISTINCT(SoldAsVacant), COUNT(SoldAsVacant)
FROM PortfolioProject.dbo.['Nashville Housing$']
GROUP BY SoldAsVacant
ORDER BY 2

SELECT SoldAsVacant,
	CASE 
		WHEN SoldAsVacant = 'Y' THEN 'Yes'
		WHEN SoldAsVacant = 'N' THEN 'No'
		ELSE SoldAsVacant
	END
FROM PortfolioProject.dbo.['Nashville Housing$']

UPDATE PortfolioProject.dbo.['Nashville Housing$']
SET SoldAsVacant = 	
	CASE 
		WHEN SoldAsVacant = 'Y' THEN 'Yes'
		WHEN SoldAsVacant = 'N' THEN 'No'
		ELSE SoldAsVacant
	END

SELECT DISTINCT(SoldAsVacant), COUNT(SoldAsVacant)
FROM PortfolioProject.dbo.['Nashville Housing$']
GROUP BY SoldAsVacant
ORDER BY 2

--------------------------------------------------------------------

-- Remove Duplicates

WITH RowNumCTE AS (
SELECT *, ROW_NUMBER() OVER (
	PARTITION BY ParcelID, 
				PropertyAddress, 
				SalePrice, 
				SaleDate, 
				LegalReference
				ORDER BY 
					UniqueID) AS row_num
FROM PortfolioProject.dbo.['Nashville Housing$']
)

DELETE
FROM RowNumCTE
WHERE row_num > 1

-------------------------------------------------------------------------

-- Delete Unused Columns (Important to not do it on original data, just an example)

SELECT *
FROM PortfolioProject.dbo.['Nashville Housing$']

ALTER TABLE PortfolioProject.dbo.['Nashville Housing$']
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate