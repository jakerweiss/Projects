/*
2022 Covid 19 Data Exploration

Skills used: Joins, CTE's, Temp. Tables, Windows Functions, Aggregate Functions, Creating Views, Converting Data types
*/


-- Checking data files
Select *
From PortfolioProject..CovidDeaths
Where continent is not null
order by 3,4

Select *
From PortfolioProject..CovidVaccinations
order by 3,4


-- Selecting starting data
Select Location, date, total_cases, new_cases, total_deaths, population
From PortfolioProject..CovidDeaths
order by 1,2


-- Looking at Total Cases vs Total Deaths
-- Shows the odds of dying from Covid in the United States
Select Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
Where location like '%states%'
and continent is not null
order by 1,2


-- Looking at Total Cases vs Population
-- Shows percentage of population infected with Covid
Select Location, date, total_cases, population, (total_cases/population)*100 as PercentPopInfect
From PortfolioProject..CovidDeaths
order by 1,2



--Countries with highest infection rate compared to population
Select Location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 as PercentPopInfect
From PortfolioProject..CovidDeaths
Group by Location, Population
order by PercentPopInfect desc


--Showing countries with highest deathcount per population
Select Location, MAX(cast(total_deaths as int)) as TotalDeathCount
From PortfolioProject..CovidDeaths
Where continent is null
Group by Location
order by TotalDeathCount desc


-- Broken down by continent 
--Showing countries with highest deathcount per population
Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
From PortfolioProject..CovidDeaths
Where continent is not null
Group by continent
order by TotalDeathCount desc


-- Global numbers
Select date, SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
Where continent is not null
group by date
order by 1,2


-- Joining tables
select *
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date


-- Total population vs vaccinations
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3


--Running total
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(convert(int, vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location,
	dea.Date) as RunningVaxTotal
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3


-- Using CTE to perform calculation
With PopvsVax(continent, location, date, population, new_vaccinations, RunningVaxTotal)
as
(
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(convert(int, vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location,
	dea.Date) as RunningVaxTotal
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
)
Select *, (RunningVaxTotal/Population)*100 as PercentPopVax
From PopvsVax


-- Using temp table instead of CTE
DROP Table if exists #PercentPopVax
Create Table #PercentPopVax
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
new_vaccinations numeric,
RunningVaxTotal numeric
)

Insert into #PercentPopVax
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(convert(int, vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location,
	dea.Date) as RunningVaxTotal
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
order by 2,3
Select *, (RunningVaxTotal/Population)*100 as PercentPopVax
From #PercentPopVax



-- Creating a view to store date
USE PortfolioProject
GO
Create View PercentPopVax as 
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(convert(int, vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location,
	dea.Date) as RunningVaxTotal
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null

--Checking View
Select*, (RunningVaxTotal/population)*100 as PercentagePopVax
From PercentPopVax


--Creating view to compare vaccination percentage vs. new cases in the US
USE PortfolioProject
GO
Create View VaxvsCases as 
select dea.continent, dea.location, dea.date, dea.population, dea.total_cases, dea.new_cases, (new_cases/total_cases)*100 as DailyPercentIncrease, vac.new_vaccinations
, SUM(convert(int, vac.new_vaccinations)) OVER (Partition by dea.Location Order by dea.location,
	dea.Date) as RunningVaxTotal
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null and dea.location  like '%states%'

Select*, (RunningVaxTotal/population)*100 as PercentagePopVax
From VaxvsCases


-- Comparing Population Density to Percentage of Population Infected
USE PortfolioProject
GO
Create View PopulationDensity as 
select dea.location, max(dea.population) as Population, MAX(cast(vac.population_density as int)) as PopDens, MAX((dea.total_cases/dea.population))*100 as PercentPopInfect
From PortfolioProject..CovidDeaths dea
Join PortfolioProject..CovidVaccinations vac
	On dea.location = vac.location
	and dea.date = vac.date 
group by dea.location

Select*
From PopulationDensity
order by PopDens desc


--Comparing overall Percentage of Population Infected vs Percentage of Population that died.
USE PortfolioProject
GO
Create View InfectVSDead as 
Select Location, max(population) as Population, max(convert(int, total_cases)/population)*100 as PercentPopInfect, max(convert(int, total_deaths)/population)*100 as PercentPopDead
From PortfolioProject..CovidDeaths
Group by location

Select*
From InfectVSDead
order by PercentPopDead desc