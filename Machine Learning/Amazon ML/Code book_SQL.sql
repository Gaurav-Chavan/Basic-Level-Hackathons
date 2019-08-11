* RetailOutlet
ROID
RName
Year_Of_Estb
Location

*SalesMan
SID
Sname
Manager
ROID

1. Display name and number of locations where retail outlets are located
2. Consider only those outlets that are not located in New Yorks and in which number of salesman is only 1.
Display the data in alphabetical order of their name


WITH
  Retail_Filtered AS (SELECT * FROM RetailOutlet where RetailOutlet.Location != 'New York'),
  Salesman_Filtered AS (SELECT SalesMan.ROID,Count(distinct(SalesMan.SID)) as num_salesman FROM SalesMan GROUP BY SalesMan.ROID Having Count(distinct(SalesMan.SID)) = 1)
  SELECT Retail_Filtered.RName, COUNT(distinct(Retail_Filtered.Location)) as num_loc 
  FROM Retail_Filtered 
  JOIN Salesman_Filtered
  ON Retail_Filtered.ROID = Salesman_Filtered.ROID 
  GROUP BY Retail_Filtered.RName
  ORDER BY Retail_Filtered.RName;
  


Select RetailOutlet.RName,COUNT(distinct(RetailOutlet.Location)) as num_loc
FROM RetailOutlet
JOIN SalesMan 
ON RetailOutlet.ROID = SalesMan.ROID
where RetailOutlet.Location NOT LIKE '[Nn][Ee][Ww] [Yy][Oo][Rr][Kk]' 
GROUP BY RetailOutlet.ROID,RetailOutlet.RName
HAVING Count(distinct(SalesMan.SID)) = 1 
ORDER BY RetailOutlet.RName ASC;



Select RetailOutlet.RName,COUNT(distinct(RetailOutlet.Location)) as num_loc
FROM RetailOutlet
JOIN  SalesMan 
ON RetailOutlet.ROID = SalesMan.ROID
where RetailOutlet.Location != '%[Nn][Ee][Ww]%[Yy][Oo][Rr][Kk]%' 
GROUP BY RetailOutlet.ROID,RetailOutlet.RName
HAVING (Count(distinct(SalesMan.SID)) = 1 AND Count(distinct(SalesMan.SName)) = 1)
ORDER BY RetailOutlet.RName,num_loc ASC;



Select RetailOutlet.rname, COUNT(distinct(RetailOutlet.Location))
FROM RetailOutlet
JOIN SalesMan 
	ON RetailOutlet.ROID = SalesMan.ROID
	and RetailOutlet.Location != 'New York'
group by RetailOutlet.rname
order by RetailOutlet.rname
having count(distinct SalesMan.SID) = 1