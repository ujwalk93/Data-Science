SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;

#ensure usage of superstores database and its tables
use superstoresdb;

#Task 1
#Superstoresdb is the database that is created to manage and store details of sales records in a superstore.
#It contains the following 5 tables:
#cust_dimen (contains details of customers who have purchased items in the superstore)
#shipping_dimen (contains details of orders that are shipped)
#orders_dimen (contains details of orders placed by the customer)
#prod_dimen (contains details about various products in the superstore and their types)
#market_fact (which is the main table containing all the details about various products, customers, orders placed, and their respective ids)
#Using appropriate queries, we can retrieve required details, such as total, average or minimum/maximum sales, and profit from the superstore database.

#The primary and foreign keys of each table are as follows:
#cust_dimen - Cust_id (primary key), no foreign key
#shipping_dimen - Ship_id (primary key), Order_ID (can be used as foreign key to refer from orders_dimen table) 
#orders_dimen - Order_ID (primary key), Ord_id (can be used as foreign key to refer from market_fact table)
#prod_dimen - Prod_id (primary key), no foreign key
#market_fact - Ord_id, Prod_id, Ship_id, Cust_id (foreign keys from tables orders_dimen, prod_dimen, shipping_dimen,
#              and cust_dimen respectively), no primary keys

#Task 2
#A. Find the total and the average sales (display total_sales and avg_sales) 
#using sum and avg aggregate functions across Sales column in market_fact table

select sum(Sales) as 'total_sales', avg(Sales) as 'avg_sales' from market_fact;

#B. Display the number of customers in each region in decreasing order of no_of_customers.The result should contain columns Region, #no_of_customers
#applying count(*) aggregate function for each region and sorting in descending order

select Region as 'region', count(*) as 'no_of_customers' from cust_dimen group by Region order by count(*) desc; 

#C. Find the region having maximum customers (display the region name and max(no_of_customers) 
#same as B but here only the top most row is extracted as the result using LIMIT 1(as TOP 1 is not supported by MYSQL)

select Region as 'region', count(*) as 'max(no_of_customers)' from cust_dimen group by Region order by count(*) desc LIMIT 1;

#D. Find the number and id of products sold in decreasing order of products sold (display product_id, no_of_products sold) 
#using sum aggregate function on Order_Quantity column and sorting in descending order

select Prod_id as 'product_id', sum(Order_Quantity) as 'no_of_products sold' from market_fact group by Prod_id order by sum(Order_Quantity) desc;

#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer #name, no_of_tables purchased)  
#combining tables using foreign key and apply appropriate constraints and aggregate functions

select c.Customer_Name as 'Customer name', sum(m.Order_Quantity) as 'no_of_tables_purchased' from cust_dimen c, market_fact m, prod_dimen p where c.Cust_id = m.Cust_id and m.Prod_id = p.Prod_id and p.Product_Sub_Category = "TABLES" and c.Region = "ATLANTIC" group by c.Customer_Name order by sum(m.Order_Quantity);

#Task 3

#A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?
#combining tables using foreign key and using sum aggregate function on Profit column and sorting in descending order

select p.Product_Category as 'product_category', sum(m.Profit) as 'Profits' from market_fact m, prod_dimen p where m.Prod_id = p.Prod_id group by p.Product_Category order by sum(m.Profit) desc;

#B. Display the product category, product sub-category and the profit within each subcategory in three columns.  
#combining tables using foreign key and grouping by Product_Sub_Category and Product_Category columns

select p.Product_Category as 'product_category', p.Product_Sub_Category as 'product_sub_category', sum(m.Profit) as 'total_profits' from market_fact m, prod_dimen p where m.Prod_id = p.Prod_id group by p.Product_Sub_Category, p.Product_Category order by total_profits;

#C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, display the  region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region) 
#combining tables using foreign key and applying appropriate constraints, aggregate functions and finally extracting the sorted output (in descending order)

select c.Region as 'region', count(m.Ship_id) as 'no_of_shipment', sum(m.Profit) as 'profit_in_each_region' from market_fact m, cust_dimen c, prod_dimen p where c.Cust_id = m.Cust_id and m.Prod_id = p.Prod_id and p.Product_Sub_Category = (select pr.Product_Sub_Category from market_fact m, prod_dimen pr where m.Prod_id = pr.Prod_id group by pr.Product_Sub_Category order by sum(m.Profit) LIMIT 1) group by c.Region order by sum(m.Profit) DESC;
#The least profitable product subcategory (TABLES) is shipped the most in Ontario.
