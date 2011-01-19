/***************************************/
/*                                     */
/*       Author: Brian Schweitzer      */
/*   Date: Tuesday, January 11, 2011   */
/*                                     */
/***************************************/

#include<iostream>
#include<string>
#include<fstream>
#include<sstream>

using namespace std;

int main()
{
	float observed[40],observedtrue[40],model[40],modeltrue[40];
	float ET;
	int date[40],datetrue[40];
	int i,j,k,count, RT;
	string input1,input2;
	ifstream meshtest;
	ifstream streamflow;
	ifstream tvstreamflow;
	ofstream testout;

	meshtest.open("MESH_test.ini");
	while(meshtest.good())
	{
		getline(cin,input1);
		i = input1.find('Error');
		j = input1.find('Run');
		if (i != string::npos)
		{
			istringstream in(input2);
			k = input1.length();
			i = input1.find('=');
			input2 = input1.substr(i + 1,k);
			in >> ET;
		}
		if (j != string::npos)
		{
			istringstream in(input2);
			k = input1.length();
			j = input1.find('=');
			input2 = input1.substr(j+1,k);
			in >> RT;
	}
	meshtest.close();

	streamflow.open("BASINAVG1\MESH_output_streamflow.csv"); 		
	count = 0;
	while(streamflow.good())
	{
		streamflow >> date[count] >> observed[count] >> model[count]; 			
		count++;
	}
	streamflow.close();

	tvstreamflow.open("BASINAVG1\True_Value_MESH_output_streamflow.csv");
	count = 0;
	while(tvstreamflow.good())
	{
		tvstreamflow >> datetrue[count] >> observedtrue[count] >> modeltrue[count];
		count++;
	}
	tvstreamflow.close();
	
	testout.open("BASINAVG1\Test_results.txt"); 
	for(int x = 0; x < 40; x++)
	{
		if (date[x] > datetrue[x] || date[x] < datetrue[x])
		{
			testout << "Dates do not match. Model has crashed. Test failed.";
			break;
		}
		if (observed[x] > observedtrue[x] || observed[x] < observedtrue[x])
		{
			testout << "Observed values do not match. Model has crashed. Test failed.";
			break;
		}
	}

	
	for(int y = 0; y < 40; y++)
	{
		if ( (y == 39 && (model[y] - modeltrue[count]) < ET) || (y == 39 && (model[y] - modeltrue[count]) > -ET))
		{
			testout << "Test Passed";
			break;
		}
		else if ( (model[count] - modeltrue[count]) >= ET || (model[count] - modeltrue[count]) <= -ET)
		{
			testout << "Test failed on day " << date[count];
			break;
		}
	}
	testout.close();	
}