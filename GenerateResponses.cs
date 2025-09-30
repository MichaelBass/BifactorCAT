using System;
using System.Configuration;
using System.Collections;
using System.Text;
using System.IO;
using System.Xml;    
//using System.Data;
//using System.Data.SqlClient;

public class GenerateResponses
{

	public static void Main(string[] args)
	{

		string param_file = args[0];
		string response_file = args[1];
		string prefix = args[2];
		

		bool IsHeader = true;
		List<string> Header = new List<string>();

		Dictionary<string, Dictionary<string, string>> Simulation = new Dictionary<string, Dictionary<string, string>>();

	    using(var reader = new StreamReader(response_file)) //@"Responses.csv"
	    {

	        while (!reader.EndOfStream)
	        {
	            var line = reader.ReadLine();

	            if(IsHeader){
		            var values = line!.Split(',');
			        foreach( string value in values)
			        {
			        	Header.Add(value);
			        }
	            	IsHeader = false;	
	            } else{
	            	Dictionary<string, string> Simulee = new Dictionary<string, string>();
					var values = line!.Split(',');
					int counter = 0;
			        foreach( string value in values)
			        {
			        	Simulee.Add(Header[counter].ToString(),value);
			        	counter++;
			        }
			        Simulation.Add(Simulee["CaseID"].ToString(),Simulee);
	            }
	        }
	    }


/*
Console.WriteLine(Simulation.Count.ToString());

		foreach(  Dictionary<string, string>test in Simulation.Values )
		{
			Console.WriteLine(test["CaseID"].ToString() + ":" + test.Count.ToString() + ":" + test["FIT_17"].ToString() );
		}
	    return;

*/

FileStream fs5 = new FileStream(prefix + @"Composite_Variance2.csv", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter sw5 = new StreamWriter(fs5,Encoding.UTF8);
sw5.WriteLine("Simulation,ItemSelection,Threshold,Theta[0],SE[0],Theta[1],SE[1],Theta[2],SE[2],Theta[3],SE[3],Var_g,Var_s1,Var_s2,Var_s3,Cov_gs[1],Cov_gs[2],Cov_gs[3]");
	
		foreach( var entry in Simulation )
		{    
			DateTime startDate = DateTime.Now;
			Dictionary<string, string>test = (Dictionary<string, string>)entry.Value;

			XmlDocument doc = new XmlDocument();
			doc.Load(param_file);
			MSS.Engines.BaseEngineGRM engine = new MSS.Engines.BaseEngineGRM(doc); // create engine

			List<string> AdministeredItems = new List<string>();
			for(int i = 0; i < test.Count; i++){
					string item_selection = engine.SelectItem_Composite_Variance2(AdministeredItems);

					if(item_selection.Equals(string.Empty)){
						break;
					}
					
					AdministeredItems.Add(item_selection);
					engine.UpdatePosteriorDistribution(entry.Key,item_selection, Int32.Parse(test[item_selection])-1,sw5);
			}
			TimeSpan diff = DateTime.Now - startDate;
			Console.WriteLine("simulation: " + entry.Key.ToString() + " Time: " + diff.Milliseconds.ToString());
		}

sw5.Flush();
sw5.Close();
fs5.Close(); 

FileStream fs4 = new FileStream(prefix + @"Composite_Variance.csv", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter sw4 = new StreamWriter(fs4,Encoding.UTF8);
sw4.WriteLine("Simulation,ItemSelection,Threshold,Theta[0],SE[0],Theta[1],SE[1],Theta[2],SE[2],Theta[3],SE[3],Var_g,Var_s1,Var_s2,Var_s3,Cov_gs[1],Cov_gs[2],Cov_gs[3]");
	
		foreach( var entry in Simulation )
		{    
			DateTime startDate = DateTime.Now;
			Dictionary<string, string>test = (Dictionary<string, string>)entry.Value;

			XmlDocument doc = new XmlDocument();
			doc.Load(param_file);
			MSS.Engines.BaseEngineGRM engine = new MSS.Engines.BaseEngineGRM(doc); // create engine

			List<string> AdministeredItems = new List<string>();
			for(int i = 0; i < test.Count; i++){
					string item_selection = engine.SelectItem_Composite_Variance(AdministeredItems);

					if(item_selection.Equals(string.Empty)){
						break;
					}
					
					AdministeredItems.Add(item_selection);
					engine.UpdatePosteriorDistribution(entry.Key,item_selection, Int32.Parse(test[item_selection])-1,sw4);
			}
			TimeSpan diff = DateTime.Now - startDate;
			Console.WriteLine("simulation: " + entry.Key.ToString() + " Time: " + diff.Milliseconds.ToString());
		}

sw4.Flush();
sw4.Close();
fs4.Close(); 


FileStream fs2 = new FileStream(prefix + @"Posterior_Variance.csv", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter sw2 = new StreamWriter(fs2,Encoding.UTF8);
sw2.WriteLine("Simulation,ItemSelection,Threshold,Theta[0],SE[0],Theta[1],SE[1],Theta[2],SE[2],Theta[3],SE[3],Var_g,Var_s1,Var_s2,Var_s3,Cov_gs[1],Cov_gs[2],Cov_gs[3]");	    
	
		foreach( var entry in Simulation )
		{    
			DateTime startDate = DateTime.Now;
			Dictionary<string, string>test = (Dictionary<string, string>)entry.Value;

			XmlDocument doc = new XmlDocument();
			doc.Load(param_file);
			MSS.Engines.BaseEngineGRM engine = new MSS.Engines.BaseEngineGRM(doc); // create engine

			List<string> AdministeredItems = new List<string>();
			for(int i = 0; i < test.Count; i++){
					string item_selection = engine.SelectItem_Sum_Posterior_Variance(AdministeredItems);
					if(item_selection.Equals(string.Empty)){
						break;
					}
					AdministeredItems.Add(item_selection);
					engine.UpdatePosteriorDistribution(entry.Key,item_selection, Int32.Parse(test[item_selection])-1,sw2);
			}
			TimeSpan diff = DateTime.Now - startDate;
			Console.WriteLine("simulation: " + entry.Key.ToString() + " Time: " + diff.Milliseconds.ToString());
		}

sw2.Flush();
sw2.Close();
fs2.Close(); 


FileStream fs3 = new FileStream(prefix + @"KL_Divergence.csv", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter sw3 = new StreamWriter(fs3,Encoding.UTF8);
sw3.WriteLine("Simulation,ItemSelection,Threshold,Theta[0],SE[0],Theta[1],SE[1],Theta[2],SE[2],Theta[3],SE[3],Var_g,Var_s1,Var_s2,Var_s3,Cov_gs[1],Cov_gs[2],Cov_gs[3]");	    
	
		foreach( var entry in Simulation )
		{    
			DateTime startDate = DateTime.Now;
			Dictionary<string, string>test = (Dictionary<string, string>)entry.Value;

			XmlDocument doc = new XmlDocument();
			doc.Load(param_file);
			MSS.Engines.BaseEngineGRM engine = new MSS.Engines.BaseEngineGRM(doc); // create engine

			List<string> AdministeredItems = new List<string>();
			for(int i = 0; i < test.Count; i++){
					string item_selection = engine.SelectItem_KL_Divergence(AdministeredItems);
					if(item_selection.Equals(string.Empty)){
						break;
					}
					AdministeredItems.Add(item_selection);
					engine.UpdatePosteriorDistribution(entry.Key,item_selection, Int32.Parse(test[item_selection])-1,sw3);
			}
			TimeSpan diff = DateTime.Now - startDate;
			Console.WriteLine("simulation: " + entry.Key.ToString() + " Time: " + diff.Milliseconds.ToString());
		}

sw3.Flush();
sw3.Close();
fs3.Close(); 


FileStream fs = new FileStream(prefix + @"MiniMax.csv", FileMode.OpenOrCreate, FileAccess.Write);
StreamWriter sw = new StreamWriter(fs,Encoding.UTF8);
sw.WriteLine("Simulation,ItemSelection,Threshold,Theta[0],SE[0],Theta[1],SE[1],Theta[2],SE[2],Theta[3],SE[3],Var_g,Var_s1,Var_s2,Var_s3,Cov_gs[1],Cov_gs[2],Cov_gs[3]");	    
	
		foreach( var entry in Simulation )
		{    
			DateTime startDate = DateTime.Now;
			Dictionary<string, string>test = (Dictionary<string, string>)entry.Value;

			XmlDocument doc = new XmlDocument();
			doc.Load(param_file);
			MSS.Engines.BaseEngineGRM engine = new MSS.Engines.BaseEngineGRM(doc); // create engine

			List<string> AdministeredItems = new List<string>();
			for(int i = 0; i < test.Count; i++){
					string item_selection = engine.SelectItem_MiniMax(AdministeredItems);
					if(item_selection.Equals(string.Empty)){
						break;
					}
					AdministeredItems.Add(item_selection);
					engine.UpdatePosteriorDistribution(entry.Key,item_selection, Int32.Parse(test[item_selection])-1,sw);
			}
			TimeSpan diff = DateTime.Now - startDate;
			Console.WriteLine("simulation: " + entry.Key.ToString() + " Time: " + diff.Milliseconds.ToString());
		}

sw.Flush();
sw.Close();
fs.Close(); 	




		//engine.GenerateResponseProbability();
/*
		double[] theta = new double[4];
		theta[0] = 1.0D;
		theta[1] = 1.0D;
		theta[2] = 1.0D;
		theta[3] = 1.0D;	
		engine.recordResponses(theta);
		

		double[,] rtn = new double[4,4];
	   rtn[0,0] = 1D;
	   rtn[0,1] = 0D;
	   rtn[0,2] = 0D;
	   rtn[0,3] = 0D;
	   rtn[1,0] = 0D;
	   rtn[1,1] = 1D;
	   rtn[1,2] = 0D;
	   rtn[1,3] = 0D;
	   rtn[2,0] = 0D;
	   rtn[2,1] = 0D;
	   rtn[2,2] = 1D;
	   rtn[2,3] = 0D;
	   rtn[3,0] = 0D;
	   rtn[3,1] = 0D;
	   rtn[3,2] = 0D;
	   rtn[3,3] = 1D;

		double[,] test = engine.Inverse44(rtn);

			for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
				Console.WriteLine("test[" + i.ToString() + "," + j.ToString() + "] = " + test[i,j].ToString());
            }
        	}

		double test2 = engine.Determinant44(rtn);
		Console.WriteLine("test2 = " + test2.ToString());


			double[,]? test3 = MSS.Engines.BaseEngineGRM.MatrixMultiply(rtn, test);

			for (int k = 0; k < 4; k++) {
            for (int l = 0; l < 4; l++) {
				Console.WriteLine("test3[" + k.ToString() + "," + l.ToString() + "] = " + test[k,l].ToString());
            }
        	}

*/
		return;
	}	
}