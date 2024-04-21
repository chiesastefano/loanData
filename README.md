# loanData

Dataset found on Kaggle: https://www.kaggle.com/datasets/saramah/loan-data?resource=download
Lending Club connects people who need money (borrowers) with people who have money (investors).

Column explanation:
- **credit.policy**: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise;
- **purpose**: The prpose of the loan ["credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", "all_other"];
- **int.rate**: The interest rate of the loan;
- **installment**: The monthly installments owed by the borrower if the loan is funded;
- **log.annual.inc**: The natural log of the self-reported annual income of the borrower (deleted);
- **dti**: The debt-to-income ratio of the borrower (amount of debt divided by annual income);
- **fico**: The FICO credit score of the borrower;
- **days.with.cr.line**: The number of days the borrower has had a credit line;
- **revol.bal**: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle);
- **revol.util**: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available); 
  A high utilization rate may indicate that the borrower is over-reliant on credit and may have trouble paying back their debts;
- **inq.last.6mths**: The borrower's number of inquiries by creditors in the last 6 months;
- **delinq.2yrs**: The number of times the borrower had been 30+ days past due on a payment in the past 2 years;
- **pub.rec**: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments);
- **not.fully.paid**: The loan is not fully paid (1) or is fully paid (0).
I then created more columns:
- **income**: exponential of _log.annual.inc_;
- **debt**: (annual income * debt-to-income ratio);
- **total_interest**: (debt * int.rate)
