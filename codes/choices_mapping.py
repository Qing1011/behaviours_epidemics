# Define the mapping from categories to numbers
expectation_mapping_1 = {
    'Things will be much better': 2,
    'Things will be somewhat better': 1,
    'Things will be about the same': 0,
    'Things will be somewhat worse': -1,
    'Things will be much worse': -2
}
expectation_mapping_2 = {
    'I always had access through my provider': 3,
    'I had to use a hotspot to access the internet': 2,
    'I only had internet access on my phone': 1,
    'I usually had access but there were some extended outages': -1,
    'I had to find public spaces to access internet': -2,
    'I did not have access at home and had to use it elsehwere': -3
}

expectation_mapping_3_4 = {
    'All my bills were paid off each month and I have enough to save, invest, or spend freely': 3,
    'I paid off my bills but was not able to save': 1,
    'I was unable to pay all of my bills': -3,
    'I had to take on some debt to pay my bills': -1,
    'I paid off my bills and saved a small amount': 2,
    'I had to take on a lot of debt to pay my bills': -2
}

expectation_mapping_5 = {
    'There were no major impacts on my financial situation': 0,
    'My financial situation became somewhat worse': -1,
    'My financial situation became much worse': -2,
    'My financial situation became much better': 2,
    'My financial situation became somewhat better': 1
}

expectation_mapping_6 = {
    'I pay off any balances every month': 2,
    'I never purchase anything on credit or take on debt': 3,
    'My debts increase beyond what I earn in a given month': -3,
    'I make the minimum payment and the interest is charged': -1,
    'I generally pay off the interest but not the balance': -2,
    'I pay off much of the balance': 1
}

expectation_mapping_8 = {
    'Man': -1,
    'Prefer not to answer': 0,
    'Woman': 1,
    'I prefer to use:': 0
}

expectation_mapping_9 = {
    'Doctoral degree': 5,
    'Graduate (Masters or equivalent)': 4,
    'Primary school': 1,
    'College or university (Bachelor or equivalent)': 3,
    'Vocational school':   2,
    'MBA': 4,
    'Secondary school (high school)': 2,
}

expectation_mapping_10 = {
    'Employed full-time (25+ hours per week)': 3,
    'Click to write Choice 7': 0,
    'Not in paid employment (by choice)': 0,
    'Not in paid employment (unable to work due to health/personal reasons)': 0,
    'Employed part-time (less than 25 hours per week)': 2,
    'Full-time student': 1,
    'Not in paid employment (looking for work)': 0,
}

expectation_mapping_11 = {
    'Asian': 1,
    'Asian,Immigrant': 1,
    'Black or African-American,Pacific Islander,Immigrant': 5,
    'Arab/Middle Eastern': 1,
    'White': 3,
    'Hispanic': 4,
    'Hispanic,Immigrant': 4,
    'Asian,White': 5,
    'Black or African-American': 2,
    'Other': 0,
    'Asian,Black or African-American': 5,
    'Arab/Middle Eastern,White': 3,
    'Prefer not to answer': 0,
    'Black or African-American,Hispanic': 5,
    'Hispanic,White': 5,
    'White,Immigrant': 3,
    'Immigrant': 0,
    'Native American,White': 5,
    'Black or African-American,Immigrant': 2,
    'Black or African-American,White': 5,
    'Black or African-American,Native American,White': 5,
    'Hispanic,Native American': 5,
    'Arab/Middle Eastern,Black or African-American,Hispanic,White': 5,
    'Arab/Middle Eastern,Pacific Islander': 5,
    'Black or African-American,Native American': 5,
    'Asian,Black or African-American,Native American,White': 5,
    'Asian,Hispanic': 5,
    'Arab/Middle Eastern,Hispanic': 4,
    'White,Other': 3,
    'Black or African-American,Hispanic,White': 5,
    'Native American,Other': 0,
    'Arab/Middle Eastern,Asian,White': 5,
    'Asian,Black or African-American,Hispanic': 5,
    'Asian,Hispanic,White': 5,
    'Arab/Middle Eastern,Asian': 1,
    'Asian,Pacific Islander,White': 5,
    'Hispanic,Other': 0,
    'Hispanic,Pacific Islander': 5,
    'Asian,Other': 0,
    'Asian,Black or African-American,Hispanic,Native American': 5,
    'Arab/Middle Eastern,Asian,Native American,White': 5,
}
