-- COVID DEATHS TABLE

-- Select the data that we are going to be used
Select Location, date, total_cases, new_cases, total_deaths, population
From PortfolioProject.dbo.CovidDeaths$
Order by 1,2


-- Looking at the Total Cases vs Total Deaths
-- Shows the likelihood of a person dying from Covid-19 infection
Select Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 As death_percentage
From PortfolioProject.dbo.CovidDeaths$
Where Location like '%states%'
Order by 1,2


-- Looking at Total Cases vs. Population
-- Shows the percentage of the population who contracted Covid-19
Select Location, date, total_cases, population, (total_cases/population)*100 As infection_rate
From PortfolioProject.dbo.CovidDeaths$
Where Location like '%states%'
Order by 1,2


-- BY COUNTRY

-- Looking at the countries with highest infection rate compared to population
-- Shows the percent of the population of all countries (descending)
Select Location, population, MAX(total_cases) As highest_infection_count, MAX((total_cases/population))*100 As percent_pop_infected
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Group by Location, population
Order by percent_pop_infected Desc


-- Looking at the countries with highest infection count compared to population
-- Shows the highest infection count of all countries that was infected (descending)
Select Location, population, MAX(total_cases) As highest_infection_count, MAX((total_cases/population))*100 As percent_pop_infected
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Group by Location, population
Order by highest_infection_count Desc


-- Looking at countries with highest death count per population
-- Shows the coutries with the highest death count per population
Select Location, MAX(Cast(total_deaths As int)) As total_death_count
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Group by Location
Order by total_death_count Desc


-- BY CONTINENT

-- Looking at continents with highest death count (by location)
-- Shows the continents with the highest death count
Select location, MAX(Cast(total_deaths As int)) As total_death_count
From PortfolioProject.dbo.CovidDeaths$
Where continent is null
Group by location
Order by total_death_count Desc

-- Looking at continents with highest death count (by continent)
-- Shows the continents with the highest death count
Select continent, MAX(Cast(total_deaths As int)) As total_death_count
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Group by continent
Order by total_death_count Desc


-- GLOBAL NUMBERS

-- Looking at the death percentage across the world by day
-- Showing the world death percentage by dividing the new deaths by new cases
Select date, SUM(new_cases) as total_cases, SUM(CAST(new_deaths As int)) as total_deaths, SUM(CAST(new_deaths As int))/SUM(new_cases) * 100 As death_perc
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Group by date
Order by 1,2

-- Looking at the death percentage across the world
-- Showing the total cases/deaths across the world, including the death percentage
Select SUM(new_cases) as total_cases, SUM(CAST(new_deaths As int)) as total_deaths, SUM(CAST(new_deaths As int))/SUM(new_cases) * 100 As death_perc
From PortfolioProject.dbo.CovidDeaths$
Where continent is not null
Order by 1,2


-- COVID VACCINATIONS TABLE

-- Looking at the Total Population vs. New Vaccinations
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
FROM PortfolioProject.dbo.CovidDeaths$ dea
Join PortfolioProject.dbo.CovidVaccinations$ vac
	On dea.location = vac.location
	and	dea.date = vac.date
Where dea.continent is not null
Order by 2,3

-- Looking at the rolling number of newly vaccinated people across each coutry
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations_smoothed,
SUM(Cast(vac.new_vaccinations_smoothed as int)) OVER (Partition by dea.location Order by 
	dea.location, dea.date) As rolling_new_vaccinations
FROM PortfolioProject.dbo.CovidDeaths$ dea
Join PortfolioProject.dbo.CovidVaccinations$ vac
	On dea.location = vac.location
	and	dea.date = vac.date
Where dea.continent is not null
Order by 2,3

-- Looking at the rollig number of newly vaccinated people across each country with rolling percentage
-- Using Common Table Expression (CTE)
With PopsvsVac (Continent, Location, Date, Population, New_Vaccinations, rolling_new_vaccinations)
as 
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(Cast(vac.new_vaccinations as int)) OVER (Partition by dea.location Order by 
	dea.location, dea.date) As rolling_new_vaccinations
FROM PortfolioProject.dbo.CovidDeaths$ dea
Join PortfolioProject.dbo.CovidVaccinations$ vac
	On dea.location = vac.location
	and	dea.date = vac.date
Where dea.continent is not null
--Order by 2,3
)
Select *, (rolling_new_vaccinations/Population)*100 As rolling_vaccinations_perc
From PopsvsVac


-- Using Temp Table
Drop Table if exists #PercentPopVaccinated
Create Table #PercentPopVaccinated
(
	Continent nvarchar(255),
	Location nvarchar(255),
	Date datetime,
	Population numeric,
	New_vaccinations numeric,
	Rolling_vaccinations numeric
)
Insert into #PercentPopVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations_smoothed,
SUM(Cast(vac.new_vaccinations_smoothed as int)) OVER (Partition by dea.location Order by 
	dea.location, dea.date) As rolling_new_vaccinations
FROM PortfolioProject.dbo.CovidDeaths$ dea
Join PortfolioProject.dbo.CovidVaccinations$ vac
	On dea.location = vac.location
	and	dea.date = vac.date
Where dea.continent is not null
--Order by 2,3

Select *, (Rolling_vaccinations/Population)*100 As rolling_vaccinations_perc
From #PercentPopVaccinated


-- Creating view to store data for later visualization
Create View PercentPopVaccinated As
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations_smoothed,
SUM(Cast(vac.new_vaccinations_smoothed as int)) OVER (Partition by dea.location Order by 
	dea.location, dea.date) As rolling_new_vaccinations
FROM PortfolioProject.dbo.CovidDeaths$ dea
Join PortfolioProject.dbo.CovidVaccinations$ vac
	On dea.location = vac.location
	and	dea.date = vac.date
Where dea.continent is not null
--Order by 2,3

Select * 
From PortfolioProject.dbo.PercentPopVaccinated