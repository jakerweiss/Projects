/* 
Data Cleaning with SQL
*/

Select *
From PortfolioProject.dbo.NashvilleHousing


--Standardizing Date Format
Select SaleDate, Convert(Date,SaleDate)
From PortfolioProject.dbo.NashvilleHousing


Update NashvilleHousing
Set SaleDate = Convert(Date,SaleDate)


Alter Table NashvilleHousing
Add SaleDateCoverted DATE;


Update NashvilleHousing
Set SaleDateCoverted = Convert(Date,SaleDate)


Select SaleDateCoverted, Convert(Date,SaleDate)
From PortfolioProject.dbo.NashvilleHousing


--Property Address Date
Select *
From PortfolioProject.dbo.NashvilleHousing
--Where PropertyAddress is null
order by ParcelID


Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From PortfolioProject.dbo.NashvilleHousing a
Join PortfolioProject.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	and a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null


Update a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From PortfolioProject.dbo.NashvilleHousing a
Join PortfolioProject.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	and a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null


--Breaking Address apart into separate columns (Address, City, State)

Select PropertyAddress
From PortfolioProject.dbo.NashvilleHousing


Select
Substring(PropertyAddress, 1, CHARINDEX(',',PropertyAddress)-1) as Address
, Substring(PropertyAddress, CHARINDEX(',',PropertyAddress)+1, LEN(PropertyAddress)) as Address
From PortfolioProject.dbo.NashvilleHousing


Alter Table NashvilleHousing
Add PropertyAddressSplit Nvarchar(255);

Update NashvilleHousing
Set PropertyAddressSplit = Substring(PropertyAddress, 1, CHARINDEX(',',PropertyAddress)-1)

Alter Table NashvilleHousing
Add PropertyCitySplit Nvarchar(255);

Update NashvilleHousing
Set PropertyCitySplit = Substring(PropertyAddress, CHARINDEX(',',PropertyAddress)+1, LEN(PropertyAddress))

Select *
From PortfolioProject.dbo.NashvilleHousing


Select OwnerAddress
From PortfolioProject.dbo.NashvilleHousing

Select
Parsename(REPLACE(OwnerAddress, ',', '.'), 3)
,Parsename(REPLACE(OwnerAddress, ',', '.'), 2)
,Parsename(REPLACE(OwnerAddress, ',', '.'), 1)
From PortfolioProject.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitAddress = Parsename(REPLACE(OwnerAddress, ',', '.'), 3)

Alter Table NashvilleHousing
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitCity = Parsename(REPLACE(OwnerAddress, ',', '.'), 2)

Alter Table NashvilleHousing
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitState = Parsename(REPLACE(OwnerAddress, ',', '.'), 1)

Select *
From PortfolioProject.dbo.NashvilleHousing


--Changing Y/N  to Yes/No in "Sold as Vacant" Field

Select Distinct(SoldAsVacant), Count(SoldAsVacant)
From PortfolioProject.dbo.NashvilleHousing
Group by SoldAsVacant
Order by 2

Select SoldAsVacant
, CASE When SoldAsVacant = 'Y' THEN 'Yes'
	   When SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END
From PortfolioProject.dbo.NashvilleHousing

Update NashvilleHousing
SET SoldAsVacant = CASE When SoldAsVacant = 'Y' THEN 'Yes'
	   When SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END


--Remove Duplicates
Select *
From PortfolioProject.dbo.NashvilleHousing


WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER(
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
					UniqueID
					) row_num

From PortfolioProject.dbo.NashvilleHousing
--order by ParcelID
)
Select *
From RowNumCTE
Where row_num > 1
Order by PropertyAddress


-- Deleting unused columns

Select *
From PortfolioProject.dbo.NashvilleHousing

ALTER TABLE PortfolioProject.dbo.NashvilleHousing
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate