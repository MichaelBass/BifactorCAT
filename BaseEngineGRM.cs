using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Linq;
using System.Xml;

namespace MSS.Engines
{
    public class BaseEngineGRM
    {
		protected static int MAX_LENGTH = 4;
        protected static int _NumTotalItems = 39;

        private string _selectionMethod = "Fischer"; 
        public string selectionMethod
        {
            get { return _selectionMethod; }
            set{ _selectionMethod = value;}
        } 

        public int itemCount
        {
            get { return _NumTotalItems; }
            set{ _NumTotalItems = value;}
        }  


 		protected static double[,] _CoVariance = new double[MAX_LENGTH,MAX_LENGTH];
        protected static double[,] _CoVarianceZero = new double[MAX_LENGTH,MAX_LENGTH];
        protected static double _LogisticScaling  = 1.0D; 
        protected static int _MaxCategories = 0;
        protected static double _ThetaIncrement = 1.0D;
        protected static double _MinTheta = -2.0D;
        protected static double _MaxTheta = 2.0D;        
        protected static Hashtable _Items = new Hashtable();
        protected static Hashtable _Domains = new Hashtable();
        protected static Hashtable _ItemIDs  = new Hashtable();
  		protected static double[] _Difficulty = new double[_NumTotalItems];
        protected static double[][] _CategoryBoundaryValues = new double[_NumTotalItems][];
        protected static double[,] _DiscriminationValues = new double[_NumTotalItems,4];
        protected static int[] _NumCategoriesByItem = new int[1];
        protected static int[] _ItemsAvailable = new int[_NumTotalItems];
        
        protected static int _NumQuadraturePoints = 20;
        protected static Point[,,,] _QuadraturePoints = new Point[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];

        protected static double[,,,] _PosteriorDistribution = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];


  		protected static double[,,,,,] _Probability = new double[_NumQuadraturePoints, _NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints, _NumTotalItems, _MaxCategories];
        protected static Hashtable CumulativeCategoryCollection  = new Hashtable();
        
        public bool init = false;
		public bool finished = false;
		public string message = string.Empty;
		protected int _DomainCount0;
		protected int _DomainCount1;
		protected int _DomainCount2;

        public BaseEngineGRM(XmlDocument doc)
        {

			if(!init){
				loadItems(doc);
				InitializeQuadrature(); // depends of loadItems

				_CoVarianceZero[0,0] = 1.0;
				_CoVarianceZero[0,1] = 0.0;
				_CoVarianceZero[0,2] = 0.0;
				_CoVarianceZero[0,3] = 0.0;

				_CoVarianceZero[1,0] = 0.0;
				_CoVarianceZero[1,1] = 1.0;
				_CoVarianceZero[1,2] = 0.0;
				_CoVarianceZero[1,3] = 0.0;

				_CoVarianceZero[2,0] = 0.0;
				_CoVarianceZero[2,1] = 0.0;
				_CoVarianceZero[2,2] = 1.0;
				_CoVarianceZero[2,3] = 0.0;

				_CoVarianceZero[3,0] = 0.0;
				_CoVarianceZero[3,1] = 0.0;
				_CoVarianceZero[3,2] = 0.0;
				_CoVarianceZero[3,3] = 1.0;
				
				_PosteriorDistribution = SetNormalDistribution(_QuadraturePoints, 0D, 1D);
				
				//PrepareProbability();


			_Theta[0] = 0D;
			_Theta[1] = 0D;
			_Theta[2] = 0D;
			_Theta[3] = 0D;			
	
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) { 	
				_Theta[0] = _Theta[0] +  ((Point)_QuadraturePoints[i,0,0,0]).x  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[1] = _Theta[1] +  ((Point)_QuadraturePoints[0,j,0,0]).y  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[2] = _Theta[2] +  ((Point)_QuadraturePoints[0,0,k,0]).z  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[3] = _Theta[3] +  ((Point)_QuadraturePoints[0,0,0,l]).w  * _PosteriorDistribution[i,j,k,l] ;				
			}
            }
			}
			}
			
			_StdError[0] = 0D;
			_StdError[1] = 0D;
			_StdError[2] = 0D;
			_StdError[3] = 0D;				
			
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) {    	
                _StdError[0] = _StdError[0] + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x  - _Theta[0],2) * _PosteriorDistribution[i,j,k,l] ;
                _StdError[1] = _StdError[1] + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[2] = _StdError[2] + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[3] = _StdError[3] + Math.Pow(((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3],2)  * _PosteriorDistribution[i,j,k,l] ;     
            }
            }
            }
        	}

            _StdError[0] =   Math.Sqrt(_StdError[0]);
            _StdError[1] =   Math.Sqrt(_StdError[1]);
            _StdError[2] =   Math.Sqrt(_StdError[2]);
            _StdError[3] =   Math.Sqrt(_StdError[3]);



				//Population 1	
				_CoVariance[0,0] = 1.0;
				_CoVariance[0,1] = 0.00; 
				_CoVariance[0,2] = 0.00; //PF-Depress = -0.40
				_CoVariance[1,0] = 0.00; //PF-PainIN = -0.78
				_CoVariance[1,1] = 1.0;
				_CoVariance[1,2] = 0.00; //PainIN-Depress = 0.54
				_CoVariance[2,0] = 0.00; //PF-Depress = -0.40
				_CoVariance[2,1] = 0.00; //PainIN-Depress = 0.54
				_CoVariance[2,2] = 1.0;			
				init = true;
			}
        }

        public double GenerateResponseProbability(int c_item, int cat )
		{

            	// initialize probability variables for each item
				int ResponseOptions = _NumCategoriesByItem[c_item];
				//double[] Probability = new double[ ResponseOptions +1];				
				double[] CumulativeCategory = new double [ ResponseOptions + 1];

					double[,,,] LikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double[,,,] PointLikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double SumLikelihoodEstimate = 0D;
					double SumPosteriorDistribution = 0D;

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						
						double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w ); 

						double P1 = 1D;
						double P2 = 0D;

						if (cat > 0){
 							P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
						} else{
							 P1 = 1D;
						}
						

						if (cat < (ResponseOptions-1)){
							P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
						} else {
							P2 = 0D;
						}
						CumulativeCategory[cat] = P1 - P2; 

						LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (CumulativeCategory[cat]);
						SumLikelihoodEstimate = SumLikelihoodEstimate  + LikelihoodEstimate[i,j,k,m];
						SumPosteriorDistribution = SumPosteriorDistribution + _PosteriorDistribution[i,j,k,m]; 
					}
					}
					}
					}

			return Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5);
		}

        public void GenerateResponseProbability()
		{
			FileStream fs = new FileStream(@"Response_Probability.csv", FileMode.OpenOrCreate, FileAccess.Write);
    		StreamWriter sw = new StreamWriter(fs,Encoding.UTF8);
			sw.WriteLine("Item,response,Probability");


            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            { 

            	// initialize probability variables for each item
				int ResponseOptions = _NumCategoriesByItem[c_item];
				//double[] Probability = new double[ ResponseOptions +1];				
				double[] CumulativeCategory = new double [ ResponseOptions + 1];

                for (int cat = 0; cat <  ResponseOptions; cat++)
                {
					double[,,,] LikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double[,,,] PointLikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double SumLikelihoodEstimate = 0D;
					double SumPosteriorDistribution = 0D;

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						
						double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w ); 

						double P1 = 1D;
						double P2 = 0D;

						if (cat > 0){
 							P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
						} else{
							 P1 = 1D;
						}
						

						if (cat < (ResponseOptions-1)){
							P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
						} else {
							P2 = 0D;
						}
						CumulativeCategory[cat] = P1 - P2; 

						LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (CumulativeCategory[cat]);
						SumLikelihoodEstimate = SumLikelihoodEstimate  + LikelihoodEstimate[i,j,k,m];
						SumPosteriorDistribution = SumPosteriorDistribution + _PosteriorDistribution[i,j,k,m]; 
					}
					}
					}
					}
				
					sw.WriteLine(_Items[c_item]?.ToString() + "," + (cat+1).ToString() + "," + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5).ToString());

                } // each response

        
            }	// each item

   			sw.Flush();
   			sw.Close();
   			fs.Close(); 
		}


        public void UpdatePosteriorDistribution(string simulation, string itemID, int cat, StreamWriter sw)
		{

			
			int c_item = (int)_ItemIDs[itemID]!;
			int ResponseOptions = _NumCategoriesByItem[c_item];

			double runningTotal = 0D;
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
			for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
			for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
			for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {

				double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w );
				double P1 = 1D;
				double P2 = 0D;

				if (cat > 0){
						P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
				} else{
					 P1 = 1D;
				}

				if (cat < (ResponseOptions-1)){
					P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
				} else {
					P2 = 0D;
				}
				_PosteriorDistribution[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (P1 - P2);
				runningTotal += _PosteriorDistribution[i,j,k,m];	
				
			}
			}
			}
			}

			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
			for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
			for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
			for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
				_PosteriorDistribution[i,j,k,m]= _PosteriorDistribution[i,j,k,m]/ runningTotal;
			}
			}
			}
			}	

			CalcThetaEstimate(simulation, itemID, cat, sw);		

		}  
       public double[,] GenerateCovariance(double[,,,] PointLikelihoodEstimate, double responseProb)
		{


			double Theta_x = 0D;
			double Theta_y = 0D;
			double Theta_z = 0D;
			double Theta_w = 0D;
	
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 
				Theta_x = Theta_x +  ((Point)_QuadraturePoints[i,0,0,0]).x  * PointLikelihoodEstimate[i,j,k,m];
				Theta_y = Theta_y +  ((Point)_QuadraturePoints[0,j,0,0]).y  * PointLikelihoodEstimate[i,j,k,m];
				Theta_z = Theta_z +  ((Point)_QuadraturePoints[0,0,k,0]).z  * PointLikelihoodEstimate[i,j,k,m];
				Theta_w = Theta_w +  ((Point)_QuadraturePoints[0,0,0,m]).w  * PointLikelihoodEstimate[i,j,k,m];	
			}
            }
			}
			}


			double[,] CoVariance = new double[4,4];
			CoVariance[0,0] = 0D;
			CoVariance[0,1] = 0D;
			CoVariance[0,2] = 0D;
			CoVariance[0,3] = 0D;

			CoVariance[1,0] = 0D;
			CoVariance[1,1] = 0D;
			CoVariance[1,2] = 0D;
			CoVariance[1,3] = 0D;

			CoVariance[2,0] = 0D;
			CoVariance[2,1] = 0D;
			CoVariance[2,2] = 0D;
			CoVariance[2,3] = 0D;

			CoVariance[3,0] = 0D;
			CoVariance[3,1] = 0D;
			CoVariance[3,2] = 0D;
			CoVariance[3,3] = 0D;

			double Variance_xx = 0D;
			double Variance_yy = 0D;
			double Variance_zz = 0D;
			double Variance_ww = 0D;

			double Variance_xy = 0D;
			double Variance_xz = 0D;
			double Variance_xw = 0D;
			double Variance_yz = 0D;
			double Variance_yw = 0D;
			double Variance_zw = 0D;



			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 

            	/*
                Variance_xx = Variance_xx + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0],2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yy = Variance_yy + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_zz = Variance_zz + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_ww = Variance_ww + Math.Pow(((Point)_QuadraturePoints[0,0,0,m]).w - _Theta[3],2) * PointLikelihoodEstimate[i,j,k,m]; 

                Variance_xy = Variance_xy + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) * PointLikelihoodEstimate[i,j,k,m];
                Variance_xz = Variance_xz + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) * PointLikelihoodEstimate[i,j,k,m];
                Variance_xw = Variance_xw + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,0,0,m]).w - _Theta[3]) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yz = Variance_yz + (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) *  (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yw = Variance_yw + (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) *  (((Point)_QuadraturePoints[0,0,0,m]).w - _Theta[3]) * PointLikelihoodEstimate[i,j,k,m];
                Variance_zw = Variance_zw + (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) *  (((Point)_QuadraturePoints[0,0,0,m]).w - _Theta[3]) * PointLikelihoodEstimate[i,j,k,m];
                */

                Variance_xx = Variance_xx + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x,2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yy = Variance_yy + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y,2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_zz = Variance_zz + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z,2) * PointLikelihoodEstimate[i,j,k,m];
                Variance_ww = Variance_ww + Math.Pow(((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w,2) * PointLikelihoodEstimate[i,j,k,m]; 

                Variance_xy = Variance_xy + ( ((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) * PointLikelihoodEstimate[i,j,k,m];
                Variance_xz = Variance_xz + ( ((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
                Variance_xw = Variance_xw + ( ((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yz = Variance_yz + ( ((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
                Variance_yw = Variance_yw + ( ((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
                Variance_zw = Variance_zw + ( ((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];                
			}
            }
			}
			}

			CoVariance[0,0] = responseProb * Variance_xx;
			CoVariance[1,1] = responseProb * Variance_yy;
			CoVariance[2,2] = responseProb * Variance_zz;
			CoVariance[3,3] = responseProb * Variance_ww;

			CoVariance[0,1] = responseProb * Variance_xy;
			CoVariance[0,2] = responseProb * Variance_xz;
			CoVariance[0,3] = responseProb * Variance_xw;
			CoVariance[1,2] = responseProb * Variance_yz;
			CoVariance[1,3] = responseProb * Variance_yw;
			CoVariance[2,3] = responseProb * Variance_zw;

			return CoVariance;

		}
       public double[,,,] ExpectedPosteriorDistribution(int c_item, int cat)
		{

			double[,,,] LikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
			int ResponseOptions = _NumCategoriesByItem[c_item];

			double runningTotal = 0D;
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
			for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
			for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
			for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {

				double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w );
				double P1 = 1D;
				double P2 = 0D;

				if (cat > 0){
						P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
				} else{
					 P1 = 1D;
				}

				if (cat < (ResponseOptions-1)){
					P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
				} else {
					P2 = 0D;
				}
				LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (P1 - P2);
				runningTotal += LikelihoodEstimate[i,j,k,m];	
				
			}
			}
			}
			}

			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
			for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
			for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
			for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
				LikelihoodEstimate[i,j,k,m] = LikelihoodEstimate[i,j,k,m]/ runningTotal;
			}
			}
			}
			}	

			return LikelihoodEstimate;		

		} 


		public void CalcThetaEstimate(string simulation, string itemID, int cat, StreamWriter sw){


			double[,] CoVariance = new double[4,4];
			CoVariance[0,0] = 0D;
			CoVariance[0,1] = 0D;
			CoVariance[0,2] = 0D;
			CoVariance[0,3] = 0D;

			CoVariance[1,0] = 0D;
			CoVariance[1,1] = 0D;
			CoVariance[1,2] = 0D;
			CoVariance[1,3] = 0D;

			CoVariance[2,0] = 0D;
			CoVariance[2,1] = 0D;
			CoVariance[2,2] = 0D;
			CoVariance[2,3] = 0D;

			CoVariance[3,0] = 0D;
			CoVariance[3,1] = 0D;
			CoVariance[3,2] = 0D;
			CoVariance[3,3] = 0D;


			double Variance_xx = 0D;
			double Variance_yy = 0D;
			double Variance_zz = 0D;
			double Variance_ww = 0D;

			double Variance_xy = 0D;
			double Variance_xz = 0D;
			double Variance_xw = 0D;
			double Variance_yz = 0D;
			double Variance_yw = 0D;
			double Variance_zw = 0D;



			_Theta[0] = 0D;
			_Theta[1] = 0D;
			_Theta[2] = 0D;
			_Theta[3] = 0D;			
	
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) { 	
				_Theta[0] = _Theta[0] +  ((Point)_QuadraturePoints[i,0,0,0]).x  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[1] = _Theta[1] +  ((Point)_QuadraturePoints[0,j,0,0]).y  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[2] = _Theta[2] +  ((Point)_QuadraturePoints[0,0,k,0]).z  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[3] = _Theta[3] +  ((Point)_QuadraturePoints[0,0,0,l]).w  * _PosteriorDistribution[i,j,k,l] ;				
			}
            }
			}
			}
			
			_StdError[0] = 0D;
			_StdError[1] = 0D;
			_StdError[2] = 0D;
			_StdError[3] = 0D;				
			
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) {    	
                _StdError[0] = _StdError[0] + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x  - _Theta[0],2) * _PosteriorDistribution[i,j,k,l] ;
                _StdError[1] = _StdError[1] + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[2] = _StdError[2] + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[3] = _StdError[3] + Math.Pow(((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3],2)  * _PosteriorDistribution[i,j,k,l] ; 

            	
                Variance_xx = Variance_xx + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0],2) * _PosteriorDistribution[i,j,k,l];
                Variance_yy = Variance_yy + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2) * _PosteriorDistribution[i,j,k,l];
                Variance_zz = Variance_zz + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2) * _PosteriorDistribution[i,j,k,l];
                Variance_ww = Variance_ww + Math.Pow(((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3],2) * _PosteriorDistribution[i,j,k,l]; 

                Variance_xy = Variance_xy + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) * _PosteriorDistribution[i,j,k,l];
                Variance_xz = Variance_xz + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) * _PosteriorDistribution[i,j,k,l];
                Variance_xw = Variance_xw + (((Point)_QuadraturePoints[i,0,0,0]).x - _Theta[0]) *  (((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3]) * _PosteriorDistribution[i,j,k,l];
                Variance_yz = Variance_yz + (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) *  (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) * _PosteriorDistribution[i,j,k,l];
                Variance_yw = Variance_yw + (((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1]) *  (((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3]) * _PosteriorDistribution[i,j,k,l];
                Variance_zw = Variance_zw + (((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2]) *  (((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3]) * _PosteriorDistribution[i,j,k,l];
               

            }
            }
            }
        	}

            _StdError[0] =   Math.Sqrt(_StdError[0]);
            _StdError[1] =   Math.Sqrt(_StdError[1]);
            _StdError[2] =   Math.Sqrt(_StdError[2]);
            _StdError[3] =   Math.Sqrt(_StdError[3]);

			sw.WriteLine(simulation + "," + itemID + "," + (cat+1).ToString() + "," + _Theta[0].ToString() + "," + _StdError[0].ToString()+ "," + _Theta[1].ToString() + "," + _StdError[1].ToString()+ "," + _Theta[2].ToString() + "," + _StdError[2].ToString()+ "," + _Theta[3].ToString() + "," + _StdError[3].ToString() + "," + Variance_ww.ToString()+ "," + Variance_xx.ToString()+ "," + Variance_yy.ToString()+ "," + Variance_zz.ToString() + "," + Variance_xw.ToString()+ "," + Variance_yw.ToString()+ "," + Variance_zw.ToString());

        }


       public string SelectItem_Composite_Variance2(List<string> AdministeredItems)
		{

			SortedList<double, string> Composite_Sum = new SortedList<double, string>();
 
            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            {   

				double[,] CoVariance = new double[4,4];
				CoVariance[0,0] = 0D;
				CoVariance[0,1] = 0D;
				CoVariance[0,2] = 0D;
				CoVariance[0,3] = 0D;

				CoVariance[1,0] = 0D;
				CoVariance[1,1] = 0D;
				CoVariance[1,2] = 0D;
				CoVariance[1,3] = 0D;

				CoVariance[2,0] = 0D;
				CoVariance[2,1] = 0D;
				CoVariance[2,2] = 0D;
				CoVariance[2,3] = 0D;

				CoVariance[3,0] = 0D;
				CoVariance[3,1] = 0D;
				CoVariance[3,2] = 0D;
				CoVariance[3,3] = 0D;

				double DomainWeight_S1 = 0D;
				double DomainWeight_S2 = 0D;
				double DomainWeight_S3 = 0D;

				int ResponseOptions = _NumCategoriesByItem[c_item];


                for (int cat = 0; cat <  ResponseOptions; cat++)
                {

					double[,,,] LikelihoodEstimate = this.ExpectedPosteriorDistribution(c_item, cat);
					double responseProb = this.GenerateResponseProbability(c_item, cat);
					double[,] PointCoVariance = this.GenerateCovariance(LikelihoodEstimate, responseProb);

					CoVariance[0,0] = CoVariance[0,0] + PointCoVariance[0,0];
					CoVariance[0,1] = CoVariance[0,1] + PointCoVariance[0,1];
					CoVariance[0,2] = CoVariance[0,2] + PointCoVariance[0,2];
					CoVariance[0,3] = CoVariance[0,3] + PointCoVariance[0,3];

					CoVariance[1,0] = CoVariance[1,0] + PointCoVariance[1,0];
					CoVariance[1,1] = CoVariance[1,1] + PointCoVariance[1,1];
					CoVariance[1,2] = CoVariance[1,2] + PointCoVariance[1,2];
					CoVariance[1,3] = CoVariance[1,3] + PointCoVariance[1,3];

					CoVariance[2,0] = CoVariance[2,0] + PointCoVariance[2,0];
					CoVariance[2,1] = CoVariance[2,1] + PointCoVariance[2,1];
					CoVariance[2,2] = CoVariance[2,2] + PointCoVariance[2,2];
					CoVariance[2,3] = CoVariance[2,3] + PointCoVariance[2,3];

					CoVariance[3,0] = CoVariance[3,0] + PointCoVariance[3,0];
					CoVariance[3,1] = CoVariance[3,1] + PointCoVariance[3,1];
					CoVariance[3,2] = CoVariance[3,2] + PointCoVariance[3,2];
					CoVariance[3,3] = CoVariance[3,3] + PointCoVariance[3,3];
		
                } // each response

				DomainWeight_S1 = Math.Pow(.40,2) * CoVariance[3,3] + Math.Pow(.60,2) * CoVariance[0,0] + (.40 * .60 * CoVariance[0,3]);
				DomainWeight_S2 = Math.Pow(.59,2) * CoVariance[3,3] + Math.Pow(.41,2) * CoVariance[1,1] + (.59 * .41 * CoVariance[1,3]);
				DomainWeight_S3 = Math.Pow(.43,2) * CoVariance[3,3] + Math.Pow(.57,2) * CoVariance[2,2] + (.43 * .57 * CoVariance[2,3]);
               	Composite_Sum.Add((DomainWeight_S1 + DomainWeight_S2 + DomainWeight_S3),_Items[c_item]?.ToString()!);

	        	
            }	// each item

			string returned_Item = string.Empty;

	        foreach( KeyValuePair<double, string> kvp in Composite_Sum)
	        {
	            if(!AdministeredItems.Contains(kvp.Value)){
	            	returned_Item  = kvp.Value;
	            	break;
	            }           
	        }

	       if( AdministeredItems.Count == 0){
	       	return string.Empty;
	       } 

	        return returned_Item;			
		}



        public string SelectItem_Composite_Variance(List<string> AdministeredItems)
		{
			KVPDoubleIntComparer3 desc = new KVPDoubleIntComparer3();
			SortedList<double, string> Composite_Sum = new SortedList<double, string>(desc);
 
            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            {   

				double[,] CoVariance = new double[4,4];
				CoVariance[0,0] = 0D;
				CoVariance[0,1] = 0D;
				CoVariance[0,2] = 0D;
				CoVariance[0,3] = 0D;

				CoVariance[1,0] = 0D;
				CoVariance[1,1] = 0D;
				CoVariance[1,2] = 0D;
				CoVariance[1,3] = 0D;

				CoVariance[2,0] = 0D;
				CoVariance[2,1] = 0D;
				CoVariance[2,2] = 0D;
				CoVariance[2,3] = 0D;

				CoVariance[3,0] = 0D;
				CoVariance[3,1] = 0D;
				CoVariance[3,2] = 0D;
				CoVariance[3,3] = 0D;

				double DomainWeight_S1 = 0D;
				double DomainWeight_S2 = 0D;
				double DomainWeight_S3 = 0D;

				int ResponseOptions = _NumCategoriesByItem[c_item];


                for (int cat = 0; cat <  ResponseOptions; cat++)
                {

					double[,,,] LikelihoodEstimate = this.ExpectedPosteriorDistribution(c_item, cat);
					double responseProb = this.GenerateResponseProbability(c_item, cat);
					double[,] PointCoVariance = this.GenerateCovariance(LikelihoodEstimate, responseProb);

					CoVariance[0,0] = CoVariance[0,0] + PointCoVariance[0,0];
					CoVariance[0,1] = CoVariance[0,1] + PointCoVariance[0,1];
					CoVariance[0,2] = CoVariance[0,2] + PointCoVariance[0,2];
					CoVariance[0,3] = CoVariance[0,3] + PointCoVariance[0,3];

					CoVariance[1,0] = CoVariance[1,0] + PointCoVariance[1,0];
					CoVariance[1,1] = CoVariance[1,1] + PointCoVariance[1,1];
					CoVariance[1,2] = CoVariance[1,2] + PointCoVariance[1,2];
					CoVariance[1,3] = CoVariance[1,3] + PointCoVariance[1,3];

					CoVariance[2,0] = CoVariance[2,0] + PointCoVariance[2,0];
					CoVariance[2,1] = CoVariance[2,1] + PointCoVariance[2,1];
					CoVariance[2,2] = CoVariance[2,2] + PointCoVariance[2,2];
					CoVariance[2,3] = CoVariance[2,3] + PointCoVariance[2,3];

					CoVariance[3,0] = CoVariance[3,0] + PointCoVariance[3,0];
					CoVariance[3,1] = CoVariance[3,1] + PointCoVariance[3,1];
					CoVariance[3,2] = CoVariance[3,2] + PointCoVariance[3,2];
					CoVariance[3,3] = CoVariance[3,3] + PointCoVariance[3,3];
		
                } // each response

				DomainWeight_S1 = Math.Pow(.40,2) * CoVariance[3,3] + Math.Pow(.60,2) * CoVariance[0,0] + (.40 * .60 * CoVariance[0,3]);
				DomainWeight_S2 = Math.Pow(.59,2) * CoVariance[3,3] + Math.Pow(.41,2) * CoVariance[1,1] + (.59 * .41 * CoVariance[1,3]);
				DomainWeight_S3 = Math.Pow(.43,2) * CoVariance[3,3] + Math.Pow(.57,2) * CoVariance[2,2] + (.43 * .57 * CoVariance[2,3]);
               	Composite_Sum.Add((DomainWeight_S1 + DomainWeight_S2 + DomainWeight_S3),_Items[c_item]?.ToString()!);

            }	// each item

			string returned_Item = string.Empty;

	        foreach( KeyValuePair<double, string> kvp in Composite_Sum)
	        {
	            if(!AdministeredItems.Contains(kvp.Value)){
	            	returned_Item  = kvp.Value;
	            	break;
	            }           
	        }

	        return returned_Item;			
		}

        public string SelectItem_KL_Divergence(List<string> AdministeredItems)
		{

			KVPDoubleIntComparer3 desc = new KVPDoubleIntComparer3();
			SortedList<double, string> KL_Trace = new SortedList<double, string>(desc);
 
            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            {   

				int ResponseOptions = _NumCategoriesByItem[c_item];
				double KL_Value_Sum = 0D;

                for (int cat = 0; cat <  ResponseOptions; cat++)
                {

					double[,,,] LikelihoodEstimate = this.ExpectedPosteriorDistribution(c_item, cat);
					double responseProb = this.GenerateResponseProbability(c_item, cat);
					double KL_Value = 0D;

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
		            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
		            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
		            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 
						KL_Value  += _PosteriorDistribution[i,j,k,m] * Math.Log(_PosteriorDistribution[i,j,k,m]/(LikelihoodEstimate[i,j,k,m]));	
					}
		            }
					}
					} 

					KL_Value_Sum +=  (KL_Value * responseProb);
		
                } // each response

                KL_Trace.Add(KL_Value_Sum,_Items[c_item]?.ToString()!);
            }	// each item

			string returned_Item = string.Empty;


	        foreach( KeyValuePair<double, string> kvp in KL_Trace)
	        {
	            if(!AdministeredItems.Contains(kvp.Value)){
	            	returned_Item  = kvp.Value;
	            	break;
	            }           
	        }

	        return returned_Item;

		}

        public string SelectItem_MiniMax(List<string> AdministeredItems)
		{
 			SortedList<double, string> Dimension_Trace = new SortedList<double, string>();

/*
			_Theta[0] = 0D;
			_Theta[1] = 0D;
			_Theta[2] = 0D;
			_Theta[3] = 0D;			
	
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) { 	
				_Theta[0] = _Theta[0] +  ((Point)_QuadraturePoints[i,0,0,0]).x  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[1] = _Theta[1] +  ((Point)_QuadraturePoints[0,j,0,0]).y  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[2] = _Theta[2] +  ((Point)_QuadraturePoints[0,0,k,0]).z  * _PosteriorDistribution[i,j,k,l] ;
				_Theta[3] = _Theta[3] +  ((Point)_QuadraturePoints[0,0,0,l]).w  * _PosteriorDistribution[i,j,k,l] ;				
			}
            }
			}
			}
			
			_StdError[0] = 0D;
			_StdError[1] = 0D;
			_StdError[2] = 0D;
			_StdError[3] = 0D;				
			
			for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
            for (int l = 0; l < _PosteriorDistribution.GetLength(3); l++) {    	
                _StdError[0] = _StdError[0] + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x  - _Theta[0],2) * _PosteriorDistribution[i,j,k,l] ;
                _StdError[1] = _StdError[1] + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[2] = _StdError[2] + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2)  * _PosteriorDistribution[i,j,k,l] ;
                _StdError[3] = _StdError[3] + Math.Pow(((Point)_QuadraturePoints[0,0,0,l]).w - _Theta[3],2)  * _PosteriorDistribution[i,j,k,l] ;     
            }
            }
            }
        	}
*/

			if(!Dimension_Trace.ContainsKey(_StdError[0])){
				Dimension_Trace.Add(_StdError[0], "CoVariance_xx");
			}
			if(!Dimension_Trace.ContainsKey(_StdError[1])){
				Dimension_Trace.Add(_StdError[1], "CoVariance_yy");
			}
			if(!Dimension_Trace.ContainsKey(_StdError[2])){
				Dimension_Trace.Add(_StdError[2], "CoVariance_zz");
			}
			if(!Dimension_Trace.ContainsKey(_StdError[3])){
				Dimension_Trace.Add(_StdError[3], "CoVariance_ww");
			}
			string dimensionSelected = Dimension_Trace.LastOrDefault().Value;

			if(AdministeredItems.Count == 0){
				dimensionSelected = "CoVariance_ww";
			} else{

				if(Dimension_Trace.Count != 4){
					Console.WriteLine("Don't have 4 domains, therefore we must have a tie");
				}
			}

 			SortedList<double, string> CoVariance_Trace = new SortedList<double, string>();

            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            {   

            	// initialize probability variables for each item
				int ResponseOptions = _NumCategoriesByItem[c_item];

				double[,] CoVariance = new double[4,4];
				CoVariance[0,0] = 0D;
				CoVariance[0,1] = 0D;
				CoVariance[0,2] = 0D;
				CoVariance[0,3] = 0D;

				CoVariance[1,0] = 0D;
				CoVariance[1,1] = 0D;
				CoVariance[1,2] = 0D;
				CoVariance[1,3] = 0D;

				CoVariance[2,0] = 0D;
				CoVariance[2,1] = 0D;
				CoVariance[2,2] = 0D;
				CoVariance[2,3] = 0D;

				CoVariance[3,0] = 0D;
				CoVariance[3,1] = 0D;
				CoVariance[3,2] = 0D;
				CoVariance[3,3] = 0D;






/*

				//double[] Probability = new double[ ResponseOptions +1];				
				double[] CumulativeCategory = new double [ ResponseOptions + 1];

					double CoVariance_xx = 0D;
					double CoVariance_yy = 0D;
					double CoVariance_zz = 0D;
					double CoVariance_ww = 0D;
					double CoVariance_xy = 0D;
					double CoVariance_xz = 0D;
					double CoVariance_xw = 0D;
					double CoVariance_yz = 0D;
					double CoVariance_yw = 0D;
					double CoVariance_zw = 0D;
*/

                for (int cat = 0; cat <  ResponseOptions; cat++)
                {
					double[,,,] LikelihoodEstimate = this.ExpectedPosteriorDistribution(c_item, cat);
					double responseProb = this.GenerateResponseProbability(c_item, cat);
					double[,] PointCoVariance = this.GenerateCovariance(LikelihoodEstimate, responseProb);

					CoVariance[0,0] = CoVariance[0,0] + PointCoVariance[0,0];
					CoVariance[0,1] = CoVariance[0,1] + PointCoVariance[0,1];
					CoVariance[0,2] = CoVariance[0,2] + PointCoVariance[0,2];
					CoVariance[0,3] = CoVariance[0,3] + PointCoVariance[0,3];

					CoVariance[1,0] = CoVariance[1,0] + PointCoVariance[1,0];
					CoVariance[1,1] = CoVariance[1,1] + PointCoVariance[1,1];
					CoVariance[1,2] = CoVariance[1,2] + PointCoVariance[1,2];
					CoVariance[1,3] = CoVariance[1,3] + PointCoVariance[1,3];

					CoVariance[2,0] = CoVariance[2,0] + PointCoVariance[2,0];
					CoVariance[2,1] = CoVariance[2,1] + PointCoVariance[2,1];
					CoVariance[2,2] = CoVariance[2,2] + PointCoVariance[2,2];
					CoVariance[2,3] = CoVariance[2,3] + PointCoVariance[2,3];

					CoVariance[3,0] = CoVariance[3,0] + PointCoVariance[3,0];
					CoVariance[3,1] = CoVariance[3,1] + PointCoVariance[3,1];
					CoVariance[3,2] = CoVariance[3,2] + PointCoVariance[3,2];
					CoVariance[3,3] = CoVariance[3,3] + PointCoVariance[3,3];
/*                	
					double[,,,] LikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double[,,,] PointLikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double SumLikelihoodEstimate = 0D;
					double SumPosteriorDistribution = 0D;

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						

						double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w ); 

						double P1 = 1D;
						double P2 = 0D;

						if (cat > 0){
 							P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
						} else{
							 P1 = 1D;
						}
						

						if (cat < (ResponseOptions-1)){
							P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
						} else {
							P2 = 0D;
						}

						CumulativeCategory[cat] = P1 - P2; 
						LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (CumulativeCategory[cat]);
						SumLikelihoodEstimate = SumLikelihoodEstimate  + LikelihoodEstimate[i,j,k,m];
						SumPosteriorDistribution = SumPosteriorDistribution + _PosteriorDistribution[i,j,k,m];

					}
					}
					}
					}

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						PointLikelihoodEstimate[i,j,k,m] = LikelihoodEstimate[i,j,k,m] / SumLikelihoodEstimate;
					}
					}
					}
					}



					double Theta_x = 0D;
					double Theta_y = 0D;
					double Theta_z = 0D;
					double Theta_w = 0D;

					double Variance_xx = 0D;
					double Variance_yy = 0D;
					double Variance_zz = 0D;
					double Variance_ww = 0D;

					double Variance_xy = 0D;
					double Variance_xz = 0D;
					double Variance_xw = 0D;
					double Variance_yz = 0D;
					double Variance_yw = 0D;
					double Variance_zw = 0D;

			
					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
		            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
		            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
		            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 
						Theta_x = Theta_x +  ((Point)_QuadraturePoints[i,0,0,0]).x  * PointLikelihoodEstimate[i,j,k,m];
						Theta_y = Theta_y +  ((Point)_QuadraturePoints[0,j,0,0]).y  * PointLikelihoodEstimate[i,j,k,m];
						Theta_z = Theta_z +  ((Point)_QuadraturePoints[0,0,k,0]).z  * PointLikelihoodEstimate[i,j,k,m];
						Theta_w = Theta_w +  ((Point)_QuadraturePoints[0,0,0,m]).w  * PointLikelihoodEstimate[i,j,k,m];				
					}
		            }
					}
					}

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
		            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
		            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
		            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 
		                Variance_xx = Variance_xx + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yy = Variance_yy + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_zz = Variance_zz + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_ww = Variance_ww+ Math.Pow(((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w,2) * PointLikelihoodEstimate[i,j,k,m]; 

		                Variance_xy = Variance_xy + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_xz = Variance_xz + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_xw = Variance_xw + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yz = Variance_yz + (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yw = Variance_yw + (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_zw = Variance_zw + (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
					}
		            }
					}
					}

					CoVariance_xx = CoVariance_xx + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xx;
					CoVariance_yy = CoVariance_yy + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yy;
					CoVariance_zz = CoVariance_zz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_zz;
					CoVariance_ww = CoVariance_ww + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_ww;

					CoVariance_xy = CoVariance_xy + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xy;
					CoVariance_xz = CoVariance_xz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xz;
					CoVariance_xw = CoVariance_xw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xw;
					CoVariance_yz = CoVariance_yz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yz;
					CoVariance_yw = CoVariance_yw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yw;
					CoVariance_zw = CoVariance_zw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_zw;
*/
                } // each response

/*
                if(dimensionSelected.Equals("CoVariance_xx")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance_xx)){
					CoVariance_Trace.Add(CoVariance_xx,_Items[c_item]?.ToString()!);
					}
                }	
                if(dimensionSelected.Equals("CoVariance_yy")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance_yy)){
					CoVariance_Trace.Add(CoVariance_yy,_Items[c_item]?.ToString()!);
					}
                }			
                if(dimensionSelected.Equals("CoVariance_zz")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance_zz)){
					CoVariance_Trace.Add(CoVariance_zz,_Items[c_item]?.ToString()!);
					}
                }	
                if(dimensionSelected.Equals("CoVariance_ww")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance_ww)){
					CoVariance_Trace.Add(CoVariance_ww,_Items[c_item]?.ToString()!);
					}
                }
*/

                if(dimensionSelected.Equals("CoVariance_xx")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance[0,0])){
					CoVariance_Trace.Add(CoVariance[0,0],_Items[c_item]?.ToString()!);
					}
                }	
                if(dimensionSelected.Equals("CoVariance_yy")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance[1,1])){
					CoVariance_Trace.Add(CoVariance[1,1],_Items[c_item]?.ToString()!);
					}
                }			
                if(dimensionSelected.Equals("CoVariance_zz")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance[2,2])){
					CoVariance_Trace.Add(CoVariance[2,2],_Items[c_item]?.ToString()!);
					}
                }	
                if(dimensionSelected.Equals("CoVariance_ww")){
                	if(!CoVariance_Trace.ContainsKey(CoVariance[3,3])){
					CoVariance_Trace.Add(CoVariance[3,3],_Items[c_item]?.ToString()!);
					}
                }


            }	// each item

   			
			string returned_Item = string.Empty;
	        foreach( KeyValuePair<double, string> kvp in CoVariance_Trace)
	        {
	            //Console.WriteLine("Key = {0}, Value = {1}",kvp.Key.ToString(), kvp.Value);
	            if(!AdministeredItems.Contains(kvp.Value)){
	            	returned_Item  = kvp.Value;
	            	break;
	            }
	        }
        
	        return returned_Item;

		}


        public string SelectItem_Sum_Posterior_Variance(List<string> AdministeredItems)
		{

 			SortedList<double, string> CoVariance_Trace = new SortedList<double, string>();

            for (int c_item = 0; c_item < _NumTotalItems; c_item++)
            {   

            	// initialize probability variables for each item
				int ResponseOptions = _NumCategoriesByItem[c_item];
				//double[] Probability = new double[ ResponseOptions +1];				
				//double[] CumulativeCategory = new double [ ResponseOptions + 1];
/*
					double CoVariance_xx = 0D;
					double CoVariance_yy = 0D;
					double CoVariance_zz = 0D;
					double CoVariance_ww = 0D;

					double CoVariance_xy = 0D;
					double CoVariance_xz = 0D;
					double CoVariance_xw = 0D;
					double CoVariance_yz = 0D;
					double CoVariance_yw = 0D;
					double CoVariance_zw = 0D;
*/


				double[,] CoVariance = new double[4,4];
				CoVariance[0,0] = 0D;
				CoVariance[0,1] = 0D;
				CoVariance[0,2] = 0D;
				CoVariance[0,3] = 0D;

				CoVariance[1,0] = 0D;
				CoVariance[1,1] = 0D;
				CoVariance[1,2] = 0D;
				CoVariance[1,3] = 0D;

				CoVariance[2,0] = 0D;
				CoVariance[2,1] = 0D;
				CoVariance[2,2] = 0D;
				CoVariance[2,3] = 0D;

				CoVariance[3,0] = 0D;
				CoVariance[3,1] = 0D;
				CoVariance[3,2] = 0D;
				CoVariance[3,3] = 0D;
	

                for (int cat = 0; cat <  ResponseOptions; cat++)
                {

	/*
					double[,,,] LikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double[,,,] PointLikelihoodEstimate = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints];
					double SumLikelihoodEstimate = 0D;
					double SumPosteriorDistribution = 0D;

				//CumulativeCategory[0] = 0D;
				//CumulativeCategory[ResponseOptions] = 1D;

					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						
				//Probability[0] = 1D;
				//Probability[ResponseOptions] = 0D;

						double sumvector = (_DiscriminationValues[c_item,0] * ((Point)_QuadraturePoints[i,0,0,0]).x + _DiscriminationValues[c_item,1] * ((Point)_QuadraturePoints[0,j,0,0]).y + _DiscriminationValues[c_item,2] * ((Point)_QuadraturePoints[0,0,k,0]).z + _DiscriminationValues[c_item,3] * ((Point)_QuadraturePoints[0,0,0,m]).w ); 

						//Probability[cat] = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );

						double P1 = 1D;
						double P2 = 0D;

						if (cat > 0){
 							P1 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat-1]) ) );
						} else{
							 P1 = 1D;
						}
						

						if (cat < (ResponseOptions-1)){
							P2 = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[c_item][cat]) ) );  // 0,1,2,3
						} else {
							P2 = 0D;
						}

						//CumulativeCategory[cat] = 1D - Probability[cat]; 
						//CumulativeCategory[cat] = Probability[cat-1] - Probability[cat];
						CumulativeCategory[cat] = P1 - P2; 


						//P(X|theta) is the likelihood of observing the responses X given ability theta
						//LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (CumulativeCategory[cat] - CumulativeCategory[cat + 1]);
						LikelihoodEstimate[i,j,k,m] = _PosteriorDistribution[i,j,k,m] * (CumulativeCategory[cat]);
						SumLikelihoodEstimate = SumLikelihoodEstimate  + LikelihoodEstimate[i,j,k,m];
						SumPosteriorDistribution = SumPosteriorDistribution + _PosteriorDistribution[i,j,k,m];

					}
					}
					}
					}



					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
					for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
					for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
					for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) {	
						PointLikelihoodEstimate[i,j,k,m] = LikelihoodEstimate[i,j,k,m] / SumLikelihoodEstimate;
					}
					}
					}
					}


*/

					double[,,,] LikelihoodEstimate = this.ExpectedPosteriorDistribution(c_item, cat);
					double responseProb = this.GenerateResponseProbability(c_item, cat);
					double[,] PointCoVariance = this.GenerateCovariance(LikelihoodEstimate, responseProb);

					CoVariance[0,0] = CoVariance[0,0] + PointCoVariance[0,0];
					CoVariance[0,1] = CoVariance[0,1] + PointCoVariance[0,1];
					CoVariance[0,2] = CoVariance[0,2] + PointCoVariance[0,2];
					CoVariance[0,3] = CoVariance[0,3] + PointCoVariance[0,3];

					CoVariance[1,0] = CoVariance[1,0] + PointCoVariance[1,0];
					CoVariance[1,1] = CoVariance[1,1] + PointCoVariance[1,1];
					CoVariance[1,2] = CoVariance[1,2] + PointCoVariance[1,2];
					CoVariance[1,3] = CoVariance[1,3] + PointCoVariance[1,3];

					CoVariance[2,0] = CoVariance[2,0] + PointCoVariance[2,0];
					CoVariance[2,1] = CoVariance[2,1] + PointCoVariance[2,1];
					CoVariance[2,2] = CoVariance[2,2] + PointCoVariance[2,2];
					CoVariance[2,3] = CoVariance[2,3] + PointCoVariance[2,3];

					CoVariance[3,0] = CoVariance[3,0] + PointCoVariance[3,0];
					CoVariance[3,1] = CoVariance[3,1] + PointCoVariance[3,1];
					CoVariance[3,2] = CoVariance[3,2] + PointCoVariance[3,2];
					CoVariance[3,3] = CoVariance[3,3] + PointCoVariance[3,3];


/*
					double Theta_x = 0D;
					double Theta_y = 0D;
					double Theta_z = 0D;
					double Theta_w = 0D;
			
					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
		            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
		            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
		            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 
						Theta_x = Theta_x +  ((Point)_QuadraturePoints[i,0,0,0]).x  * PointLikelihoodEstimate[i,j,k,m];
						Theta_y = Theta_y +  ((Point)_QuadraturePoints[0,j,0,0]).y  * PointLikelihoodEstimate[i,j,k,m];
						Theta_z = Theta_z +  ((Point)_QuadraturePoints[0,0,k,0]).z  * PointLikelihoodEstimate[i,j,k,m];
						Theta_w = Theta_w +  ((Point)_QuadraturePoints[0,0,0,m]).w  * PointLikelihoodEstimate[i,j,k,m];	
					}
		            }
					}
					}

					double Variance_xx = 0D;
					double Variance_yy = 0D;
					double Variance_zz = 0D;
					double Variance_ww = 0D;

					double Variance_xy = 0D;
					double Variance_xz = 0D;
					double Variance_xw = 0D;
					double Variance_yz = 0D;
					double Variance_yw = 0D;
					double Variance_zw = 0D;



					for (int i = 0; i < _PosteriorDistribution.GetLength(0); i++) {
		            for (int j = 0; j < _PosteriorDistribution.GetLength(1); j++) {
		            for (int k = 0; k < _PosteriorDistribution.GetLength(2); k++) {
		            for (int m = 0; m < _PosteriorDistribution.GetLength(3); m++) { 

		                Variance_xx = Variance_xx + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yy = Variance_yy + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_zz = Variance_zz + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z,2) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_ww = Variance_ww+ Math.Pow(((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w,2) * PointLikelihoodEstimate[i,j,k,m]; 

		                Variance_xy = Variance_xy + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_xz = Variance_xz + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_xw = Variance_xw + (((Point)_QuadraturePoints[i,0,0,0]).x - Theta_x) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yz = Variance_yz + (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_yw = Variance_yw + (((Point)_QuadraturePoints[0,j,0,0]).y - Theta_y) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];
		                Variance_zw = Variance_zw + (((Point)_QuadraturePoints[0,0,k,0]).z - Theta_z) *  (((Point)_QuadraturePoints[0,0,0,m]).w - Theta_w) * PointLikelihoodEstimate[i,j,k,m];

					}
		            }
					}
					}


					CoVariance_xx = CoVariance_xx + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xx;
					CoVariance_yy = CoVariance_yy + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yy;
					CoVariance_zz = CoVariance_zz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_zz;
					CoVariance_ww = CoVariance_ww + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_ww;

					CoVariance_xy = CoVariance_xy + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xy;
					CoVariance_xz = CoVariance_xz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xz;
					CoVariance_xw = CoVariance_xw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_xw;
					CoVariance_yz = CoVariance_yz + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yz;
					CoVariance_yw = CoVariance_yw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_yw;
					CoVariance_zw = CoVariance_zw + Math.Round(SumLikelihoodEstimate/SumPosteriorDistribution ,5) * Variance_zw;
*/

                } // each response

				double key = 0D;
				string _value = string.Empty;

				key = CoVariance[0,0]  + CoVariance[1,1]  + CoVariance[2,2]  + CoVariance[3,3];
				//key = CoVariance_xx + CoVariance_yy + CoVariance_zz + CoVariance_ww;
				CoVariance_Trace.Add(key!,_Items[c_item]?.ToString()!);


            }	// each item


   			string returned_Item = string.Empty;
	        foreach( KeyValuePair<double, string> kvp in CoVariance_Trace)
	        {
	            //Console.WriteLine("Key = {0}, Value = {1}",kvp.Key.ToString(), kvp.Value);
	            if(!AdministeredItems.Contains(kvp.Value)){
	            	returned_Item  = kvp.Value;
	            	break;
	            }
	        }
        

	        return returned_Item;


		}

        protected double[] _StdError = new double[MAX_LENGTH]; 
        public double[] StdError
        {
            get { return _StdError; }
            set{ _StdError = value;}
        } 
        protected double[] _Theta = new double[MAX_LENGTH];
        public double[] Theta
        {
            get { return _Theta; }
            set{ _Theta = value;}
        }
        
        protected double _Variance;
        public double Variance
        {
            get { return _Variance; }
            set{ _Variance = value;}
        }
 
        protected ArrayList _ItemsAsked  = new ArrayList();
        protected ArrayList _Responses  = new ArrayList();
   

        protected double[,,,] _LikelihoodEstimate = new double[_NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints];
        protected double[,,,] _PosteriorProbability = new double[_NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints]; 

 
		public void loadItems(XmlDocument FormParams){

				_ItemIDs.Clear();
				_Items.Clear();
				_Domains.Clear();


			XmlNodeList Properties  = FormParams.GetElementsByTagName("Property");
			for (int i = 0; i < Properties.Count; i++)
            {
				if(Properties[i]?.Attributes?["Name"]?.Value == "ThetaIncrement"){
					_ThetaIncrement = Convert.ToDouble(Properties[i]?.Attributes?["Value"]?.Value);	
				}
				if(Properties[i]?.Attributes?["Name"]?.Value == "MinTheta"){
					_MinTheta = Convert.ToDouble(Properties[i]?.Attributes?["Value"]?.Value);	
				}
				if(Properties[i]?.Attributes?["Name"]?.Value == "MaxTheta"){
					_MaxTheta = Convert.ToDouble(Properties[i]?.Attributes?["Value"]?.Value);	
				}
            }
			
			XmlNodeList CATitems;

			ArrayList arItemsAvailable = new ArrayList();
			ArrayList arNumCatByItem = new ArrayList();
			ArrayList arDisc = new ArrayList();
			ArrayList grDisc = new ArrayList();
			ArrayList arDomain = new ArrayList();
			List<double> liDiff = new List<double>();
			ArrayList arCBV = new ArrayList();
			ArrayList arItemCBV;
			ArrayList arNumCat = new ArrayList();
			
			int j;
			//Get item CAT parameters

			CATitems = FormParams.SelectNodes("descendant::Item")!;	
	
//Console.WriteLine("Itemcount: " + CATitems.Count.ToString());
				
			//int itemcount = 0;
			for (int i = 0; i < CATitems.Count; i++)
			{
				XmlNode CATitem2 = CATitems[i]!;
	
				_ItemIDs.Add(CATitems[i]?.Attributes?["VariableName"]?.Value.ToUpper()!, i);
				_Items.Add(i, CATitems[i]?.Attributes?["VariableName"]?.Value.ToUpper()!);
				_Domains.Add(CATitems[i]?.Attributes?["VariableName"]?.Value.ToUpper()!, CATitems[i]?.Attributes?["Domain"]?.Value.ToUpper()!);
				
				arItemsAvailable.Add(1);


				grDisc.Add(Convert.ToDouble(CATitem2.Attributes?["G_GRM"]?.Value));

				 //Item Discrimination value (slope or A)
				arDisc.Add(Convert.ToDouble(CATitem2.Attributes?["A_GRM"]?.Value));
				arDomain.Add(Convert.ToInt32(CATitem2.Attributes?["Domain"]?.Value));
				//Category boundary values
				arItemCBV = new ArrayList();
				//Position loading of Threshold based on Value
				//Assumes that the category boundary values are always in the correct order in the XML document
				j = 1;
				foreach (XmlNode n in CATitem2.SelectNodes("Map")!)
				{
					//Note: j = number of categories, or one less than the number of category boundary values
					int desc;
					//Check for non-integer step order (might happen with skip or invalid responses)
					if (int.TryParse(n.Attributes?["StepOrder"]?.Value, out desc))
					{
						//This takes into account collapsed categories
						arItemCBV.Add(Convert.ToDouble(n.Attributes?["Value"]?.Value));
						j++;
					}
				}
				arCBV.Add((double[])arItemCBV.ToArray(typeof(double)));
				//Number of categories is one more than the number of category boundary values: ending value from FOR loop
				arNumCat.Add(j);
				if (j > _MaxCategories)
				{
					_MaxCategories = j;
				}
			}

			_DiscriminationValues = new double[arDisc.Count,4];
			for( int k=0; k < arDisc.Count; k++){
			
				if((Int32)arDomain[k]! == 1){
				_DiscriminationValues[k,0] = (double) arDisc[k]!;
				_DiscriminationValues[k,1] = 0.0;
				_DiscriminationValues[k,2] = 0.0;
				_DiscriminationValues[k,3] =(double) grDisc[k]!;
				}
				if((Int32)arDomain[k]! == 2){
				_DiscriminationValues[k,0] = 0.0;
				_DiscriminationValues[k,1] =(double) arDisc[k]!;
				_DiscriminationValues[k,2] = 0.0;
				_DiscriminationValues[k,3] =(double) grDisc[k]!;				
				}
				if((Int32)arDomain[k]! == 3){
				_DiscriminationValues[k,0] = 0.0;
				_DiscriminationValues[k,1] = 0.0;
				_DiscriminationValues[k,2] =(double) arDisc[k]!;
				_DiscriminationValues[k,3] =(double) grDisc[k]!;				
				}
			}			
			_Difficulty = liDiff.ToArray();
			_CategoryBoundaryValues = (double[][])arCBV.ToArray(typeof(double[]));
			_NumCategoriesByItem = (int[])arNumCat.ToArray(typeof(int));
			_ItemsAvailable = (int[])arItemsAvailable.ToArray(typeof(int));
			_NumTotalItems = _ItemIDs.Count;

		}
		
        private void PrepareProbability()
        {
 
    //FileStream fs = new FileStream(@"Probability.txt", FileMode.OpenOrCreate, FileAccess.Write);
    //StreamWriter sw = new StreamWriter(fs,Encoding.UTF8);
	
            int qpnt;
            int qpnt2;
            int qpnt3;
            int qpnt4;
            int item;
            int cat;

            _Probability = new double[_NumQuadraturePoints, _NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints, _NumTotalItems, _MaxCategories];

            for (item = 0; item < _NumTotalItems; item++)
            {        
                double[,,,,] CumulativeCategory = new double[_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumQuadraturePoints,_NumCategoriesByItem[item] + 1];
                for (qpnt = 0; qpnt < CumulativeCategory.GetLength(0); qpnt++){
                for (qpnt2 = 0; qpnt2 < CumulativeCategory.GetLength(1); qpnt2++){
                for (qpnt3 = 0; qpnt3 < CumulativeCategory.GetLength(2); qpnt3++){
                for (qpnt4 = 0; qpnt4 < CumulativeCategory.GetLength(3); qpnt4++){	
                    CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, 0] = 1;
                    CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4,_NumCategoriesByItem[item]] = 0; //already zero by initialization
                }
                }
                }
            	}
                
                for (cat = 0; cat < _NumCategoriesByItem[item] - 1; cat++)
                {
                    for (qpnt = 0; qpnt < CumulativeCategory.GetLength(0); qpnt++){ 
                    for (qpnt2 = 0; qpnt2 < CumulativeCategory.GetLength(1); qpnt2++){               
                    for (qpnt3 = 0; qpnt3 < CumulativeCategory.GetLength(2); qpnt3++){ 
                	for (qpnt4 = 0; qpnt4 < CumulativeCategory.GetLength(3); qpnt4++){                           
                       Point p = (Point)_QuadraturePoints[qpnt, qpnt2, qpnt3, qpnt4];
                       double [] locationvector = new double[4];
                       locationvector[0] = p.x - _CategoryBoundaryValues[item][cat];
                       locationvector[1] = p.y - _CategoryBoundaryValues[item][cat];  
                       locationvector[2] = p.z - _CategoryBoundaryValues[item][cat];
                       locationvector[3] = p.w - _CategoryBoundaryValues[item][cat];
                       double sumvector = (_DiscriminationValues[item,0] * locationvector[0] + _DiscriminationValues[item,1] * locationvector[1] + _DiscriminationValues[item,2] * locationvector[2] + _DiscriminationValues[item,3] * locationvector[3]); 
                       CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, cat + 1] = 1 / (1 + Math.Exp((-1) * _LogisticScaling * sumvector));
					}
                    }
                    }
                	}
                }

                for (qpnt = 0; qpnt < _Probability.GetLength(0); qpnt++){
                for (qpnt2 = 0; qpnt2 < _Probability.GetLength(1); qpnt2++){
                for (qpnt3 = 0; qpnt3 < _Probability.GetLength(2); qpnt3++){ 
                for (qpnt4 = 0; qpnt4 < _Probability.GetLength(3); qpnt4++){  
                    _Probability[qpnt,qpnt2,qpnt3,qpnt4, item, 0] = 1 - CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, 0];
                    _Probability[qpnt,qpnt2,qpnt3,qpnt4, item, _NumCategoriesByItem[item] - 1] = CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, _NumCategoriesByItem[item] - 1];                  
                }
				}
				}
				}

                for (cat = 0; cat < _NumCategoriesByItem[item]; cat++)
                {
                    for (qpnt = 0; qpnt < _Probability.GetLength(0); qpnt++){
                    for (qpnt2 = 0; qpnt2 < _Probability.GetLength(1); qpnt2++){
                    for (qpnt3 = 0; qpnt3 < _Probability.GetLength(2); qpnt3++){
                    for (qpnt4 = 0; qpnt4 < _Probability.GetLength(3); qpnt4++){ 	
                        _Probability[qpnt,qpnt2,qpnt3,qpnt4, item, cat] = CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, cat] - CumulativeCategory[qpnt,qpnt2,qpnt3,qpnt4, cat + 1];
//sw.WriteLine("Probability[x=" + qpnt.ToString() + ",y=" + qpnt2.ToString() + ",z=" + qpnt3.ToString() + ", item=" + item.ToString() + ", cat=" + cat.ToString() + "]: " + item.ToString() + ":" + _Probability[qpnt,qpnt2,qpnt3, item, cat].ToString());                        
					}
                    }
                    }
                	}
                }
                
                CumulativeCategoryCollection[item] = CumulativeCategory;


//Console.WriteLine("Item: " + item.ToString() + ":" + _NumCategoriesByItem[item].ToString());
		                
            }

   //sw.Flush();
   //sw.Close();
   //fs.Close();

        }

        public virtual void initializeTest(){}
        public virtual string? getCurrentItem(int k){ return null;}
        
        public virtual void updateNode(string? FormItemOID, int ResponseIndex)
        {
				if (Int32.Parse( _Domains[FormItemOID!]!.ToString()! ) == 0){
					this._DomainCount0 = this._DomainCount0 + 1;
				}
				if (Int32.Parse( _Domains[FormItemOID!]!.ToString()! ) == 1){
					this._DomainCount1 = this._DomainCount1 + 1;
				}
				if (Int32.Parse( _Domains[FormItemOID!]!.ToString()! ) == 2){
					this._DomainCount2 = this._DomainCount2 + 1;
				}

			int ItemIndex;
			int NumAnsweredItems;

			ItemIndex = (int)_ItemIDs[FormItemOID!]!;
			_ItemsAsked.Add(ItemIndex);
			//Note: this is the collapsed value if category is collapsed; -1 if skipped or invalid
			_Responses.Add(ResponseIndex);
			_ItemsAvailable[ItemIndex] = 0;
			NumAnsweredItems = _ItemsAsked.Count;

			//estimate new theta
			CalcThetaEstimate( _ItemsAsked.Count );

		if(this._DomainCount0 > MAX_LENGTH && this._DomainCount1 > MAX_LENGTH && this._DomainCount2 > MAX_LENGTH ){
			finished = true;
		}
        }
     
        protected void InitializeLikelihood()
        {          
            if (_NumQuadraturePoints == 0)
            {
				return;
            }
 
            _LikelihoodEstimate = new double[_NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints];
            _PosteriorProbability = new double[_NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints];
           
            Point estPoint = new Point(_Theta[0], _Theta[1], _Theta[2], _Theta[3]);
            
            for (int i = 0; i < _NumQuadraturePoints; i++){
			for (int j = 0; j < _NumQuadraturePoints; j++){
			for (int k = 0; k < _NumQuadraturePoints; k++){	
			for (int l = 0; l < _NumQuadraturePoints; l++){			
				_LikelihoodEstimate[i,j,k,l] = _PriorDistribution[i,j,k,l]; 
				_PosteriorProbability[i,j,k,l] = _PriorDistribution[i,j,k,l];           
			}
			}
			}
			}

        }		
        protected double[,,,] _PriorDistribution = new double[_NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints];
        protected double _PriorDistributionMean = 0D;
        protected double _PriorDistributionStdDev = 1D;
        

        // TODO:  Need to update for 4Dimensional array if using Information for item selection
        protected double[,,,] SetNormalDistribution(Point[,,,] DataArray, double Mean, double StdDev)
        {

            if (Mean.Equals(null)) {
                Mean = 0.0;
            }
            if (StdDev.Equals(null)) {
                StdDev = 1.0;
            }
            else if (StdDev <= 0.0) {
                StdDev = 1.0;
            }
            
            double[,,,] ar1d = new double[DataArray.GetLength(0), DataArray.GetLength(1), DataArray.GetLength(2), DataArray.GetLength(3)];
            double tmp;

            double detMatrix  =  Determinant44(_CoVarianceZero);
            double sum = 0.0D;
  
            double[,] inverse =  Inverse44(_CoVarianceZero);

			for (int i = 0; i < DataArray.GetLength(0); i++) {
			for (int j = 0; j < DataArray.GetLength(1); j++) {
			for (int k = 0; k < DataArray.GetLength(2); k++) {
			for (int m = 0; m < DataArray.GetLength(3); m++) {	
				Point theta = DataArray[i,j,k,m];
				double x = theta.x;
				double y = theta.y;	
				double z = theta.z;	
				double w = theta.w;	
				tmp = -1*(x*x*inverse[0,0] + x*y*inverse[0,1] + x*z*inverse[0,2] + x*w*inverse[0,3] + y*x*inverse[1,0] + y*y*inverse[1,1] + y*z*inverse[1,2] + y*w*inverse[1,3] + z*x*inverse[2,0] + z*y*inverse[2,1] + z*z*inverse[2,2] + z*w*inverse[2,3] + w*x*inverse[3,0] + w*y*inverse[3,1] + w*z*inverse[3,2] + w*w*inverse[3,3] )/2;
				ar1d[i,j,k,m] = Math.Pow((2 * Math.PI),-4/2)*Math.Pow(detMatrix,-.5) * Math.Exp(tmp) ;

				// sw.WriteLine("(" + x.ToString() + "," + y.ToString() + "," + z.ToString() + "," +  w.ToString() + ") = " + ar1d[i,j,k,m].ToString() );
				sum = sum + ar1d[i,j,k,m];
			}
			}
			}
			}

			for (int i = 0; i < DataArray.GetLength(0); i++) {
			for (int j = 0; j < DataArray.GetLength(1); j++) {
			for (int k = 0; k < DataArray.GetLength(2); k++) {
			for (int m = 0; m < DataArray.GetLength(3); m++) {	
				ar1d[i,j,k,m] = ar1d[i,j,k,m]/sum;
			}
			}
			}
			}

            return ar1d;
        }
        
        protected static void InitializeQuadrature()
        {
            int i;
            //Calculate quadrature points
            ArrayList Points = new ArrayList();
            double pnt = _MinTheta;

            //Assume 20 decimal places
            double Pow10;
            i = 1;
            double OnePercent = _ThetaIncrement * 0.01;
            do
            {
                i--;
                Pow10 = Math.Pow(10, i);
            } while ((i > -20) && (Pow10 > OnePercent));
            int RoundingPlace = -1 * i;


            do
            {
                Points.Add(pnt);
                pnt = Math.Round((pnt + _ThetaIncrement), RoundingPlace);
            } while (pnt <= _MaxTheta);

            //Calculate number of quadrature points
            _NumQuadraturePoints = Points.Count;
            _QuadraturePoints  = new Point[ _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints, _NumQuadraturePoints];

			for(int k = 0; k < _NumQuadraturePoints; k++){
			for(int j = 0; j < _NumQuadraturePoints; j++){
			for(int l = 0; l < _NumQuadraturePoints; l++){
			for(int m = 0; m < _NumQuadraturePoints; m++){				
				Point p = new Point((double)Points[k]!, (double)Points[j]!, (double)Points[l]!, (double)Points[m]!);
				_QuadraturePoints[k,j,l,m] = p;
			} 
			}
			}
			}

        }
         
        protected void CalcThetaEstimate(int itemID)
        {


            double[,,,] QAProbability = new double[_QuadraturePoints.GetLength(0), _QuadraturePoints.GetLength(1), _QuadraturePoints.GetLength(2), _QuadraturePoints.GetLength(3)];
            
			for (int i = 0; i < _Probability.GetLength(0); i++) {
			for (int j = 0; j < _Probability.GetLength(1); j++) {
			for (int k = 0; k < _Probability.GetLength(2); k++) {
			for (int l = 0; l < _Probability.GetLength(3); l++) {				
				QAProbability[i,j,k,l] = _Probability[i,j,k,l, (int)_ItemsAsked[_ItemsAsked.Count - 1]!, (int)_Responses[_Responses.Count - 1]!];
				
				//if(QAProbability[i,j,k] == 0D){
				//	QAProbability[i,j,k] = Double.Epsilon;
				//}
				
			}
			}  
			}  
			}

			for (int i = 0; i < _LikelihoodEstimate.GetLength(0); i++) {
			for (int j = 0; j < _LikelihoodEstimate.GetLength(1); j++) {
			for (int k = 0; k < _LikelihoodEstimate.GetLength(2); k++) {
			for (int l = 0; l < _LikelihoodEstimate.GetLength(3); l++) {			
				//sw.WriteLine( "(" + i.ToString() + "," + j.ToString() + "," + k.ToString() + ")\t\t" + _LikelihoodEstimate[i,j,k].ToString() + "\t\t" +  QAProbability[i,j,k].ToString() + "\t\t" + (_LikelihoodEstimate[i,j,k] * QAProbability[i,j,k]).ToString()  );
				
				
				_LikelihoodEstimate[i,j,k,l] = _LikelihoodEstimate[i,j,k,l] * QAProbability[i,j,k,l];	
				
				//if(_LikelihoodEstimate[i,j,k] == 0D){
				//	_LikelihoodEstimate[i,j,k] = Double.Epsilon;
				//}
			}
			}
			}
			}

   //sw.Flush();
   // sw.Close();
   // fs.Close();
	
			double[,,,] h = new double[_QuadraturePoints.GetLength(0), _QuadraturePoints.GetLength(1), _QuadraturePoints.GetLength(2), _QuadraturePoints.GetLength(3)];
			double  denominatorH = ArraySum(_LikelihoodEstimate);

			for (int i = 0; i < _LikelihoodEstimate.GetLength(0); i++) {
			for (int j = 0; j < _LikelihoodEstimate.GetLength(1); j++) {
			for (int k = 0; k < _LikelihoodEstimate.GetLength(2); k++) {
			for (int l = 0; l < _LikelihoodEstimate.GetLength(3); l++) {	
				h[i,j,k,l] = _LikelihoodEstimate[i,j,k,l] / denominatorH;
			}
			}
			}
			}
	
			_PosteriorProbability = h;
			
			_Theta[0] = 0D;
			_Theta[1] = 0D;
			_Theta[2] = 0D;
			_Theta[3] = 0D;			
	
			for (int i = 0; i < _LikelihoodEstimate.GetLength(0); i++) {
            for (int j = 0; j < _LikelihoodEstimate.GetLength(1); j++) {
            for (int k = 0; k < _LikelihoodEstimate.GetLength(2); k++) {
            for (int l = 0; l < _LikelihoodEstimate.GetLength(3); l++) { 	
				_Theta[0] = _Theta[0] +  ((Point)_QuadraturePoints[i,0,0,0]).x  * h[i,j,k,l] ;
				_Theta[1] = _Theta[1] +  ((Point)_QuadraturePoints[0,j,0,0]).y  * h[i,j,k,l] ;
				_Theta[2] = _Theta[2] +  ((Point)_QuadraturePoints[0,0,k,0]).z  * h[i,j,k,l] ;
				_Theta[3] = _Theta[3] +  ((Point)_QuadraturePoints[0,0,0,k]).w  * h[i,j,k,l] ;				
			}
            }
			}
			}
			
			_StdError[0] = 0D;
			_StdError[1] = 0D;
			_StdError[2] = 0D;
			_StdError[3] = 0D;				
			
			for (int i = 0; i < _LikelihoodEstimate.GetLength(0); i++) {
            for (int j = 0; j < _LikelihoodEstimate.GetLength(1); j++) {
            for (int k = 0; k < _LikelihoodEstimate.GetLength(2); k++) {
            for (int l = 0; l < _LikelihoodEstimate.GetLength(3); l++) {    	
                _StdError[0] = _StdError[0] + Math.Pow(((Point)_QuadraturePoints[i,0,0,0]).x  - _Theta[0],2) * h[i,j,k,l] ;
                _StdError[1] = _StdError[1] + Math.Pow(((Point)_QuadraturePoints[0,j,0,0]).y - _Theta[1],2)  * h[i,j,k,l] ;
                _StdError[2] = _StdError[2] + Math.Pow(((Point)_QuadraturePoints[0,0,k,0]).z - _Theta[2],2)  * h[i,j,k,l] ;
                _StdError[3] = _StdError[3] + Math.Pow(((Point)_QuadraturePoints[0,0,0,k]).w - _Theta[3],2)  * h[i,j,k,l] ;     
            }
            }
            }
        	}

            _StdError[0] =   Math.Sqrt(_StdError[0]);
            _StdError[1] =   Math.Sqrt(_StdError[1]);
            _StdError[2] =   Math.Sqrt(_StdError[2]);
            _StdError[3] =   Math.Sqrt(_StdError[3]);




			if(_StdError[0] < 0.3D && _StdError[1] < 0.3D && _StdError[2] < 0.3D ){
				finished = true;
			}



        }
        
        protected double Determinant22(double[,] input){        
			return (input[0,0] * input[1,1]) - (input[1,0] * input[0,1]);
        }
        
        protected double Determinant33(double[,] input){
        
			double d1 = input[0,0] * ((input[2,2] * input[1,1]) - (input[1,2] * input[2,1]));
			double d2 = -1D * input[1,0] * ((input[0,1] * input[2,2]) - (input[2,1] * input[0,2]));
            double d3 = input[2,0] * ((input[0,1] * input[1,2]) - (input[1,1] * input[0,2]));
			return d1 + d2 + d3;
        }  

        public double Determinant44(double[,] input){
        
			var A2323 = input[2,2] * input[3,3] - input[2,3] * input[3,2];
			var A1323 = input[2,1] * input[3,3] - input[2,3] * input[3,1];
			var A1223 = input[2,1] * input[3,2] - input[2,2] * input[3,1];
			var A0323 = input[2,0] * input[3,3] - input[2,3] * input[3,0];
			var A0223 = input[2,0] * input[3,2] - input[2,2] * input[3,0];
			var A0123 = input[2,0] * input[3,1] - input[2,1] * input[3,0];
			var A2313 = input[1,2] * input[3,3] - input[1,3] * input[3,2];
			var A1313 = input[1,1] * input[3,3] - input[1,3] * input[3,1];
			var A1213 = input[1,1] * input[3,2] - input[1,2] * input[3,1];
			var A2312 = input[1,2] * input[2,3] - input[1,3] * input[2,2];
			var A1312 = input[1,1] * input[2,3] - input[1,3] * input[2,1];
			var A1212 = input[1,1] * input[2,2] - input[1,2] * input[2,1];
			var A0313 = input[1,0] * input[3,3] - input[1,3] * input[3,0];
			var A0213 = input[1,0] * input[3,2] - input[1,2] * input[3,0];
			var A0312 = input[1,0] * input[2,3] - input[1,3] * input[2,0];
			var A0212 = input[1,0] * input[2,2] - input[1,2] * input[2,0];
			var A0113 = input[1,0] * input[3,1] - input[1,1] * input[3,0];
			var A0112 = input[1,0] * input[2,1] - input[1,1] * input[2,0];

			var det = input[0,0] * ( input[1,1] * A2323 - input[1,2]  * A1323 + input[1,3]  * A1223 ) - input[0,1] * ( input[1,0] * A2323 - input[1,2] * A0323 + input[1,3] * A0223 ) + input[0,2] * ( input[1,0]  * A1323 -  input[1,1] * A0323 + input[1,3]* A0123 ) - input[0,3] * ( input[1,0]  * A1223 -  input[1,1] * A0223 + input[1,2] * A0123 );

			return det; 
			/*
 			double[,] a11 = new double[3,3];
 			double[,] a12 = new double[3,3];
 			double[,] a13 = new double[3,3];
 			double[,] a14 = new double[3,3];

			for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {

             	a11[i,j] = input[i+1,j+1];

            	int k = i;
            	int l = j;

            	if(i > 0){
					k = i + 1;
            	}
                if(j > 0){
            		l = j + 1;
            	}
            	a12[i,j] = input[k,l];

            	int m = i;
            	int n = j
            	if(i > 1){
					m = i + 1;
            	}
                if(j > 1){
            		n = j + 1;
            	}
            	a13[i,j] = input[m,n];


            	int o = i;
            	int p = j
            	if(i > 1){
					o = i + 1;
            	}
                if(j > 1){
            		p = j + 1;
            	}
            	a14[i,j] = input[o,p];

            }	
        	}

            return = input[0,0] * Determinant33(a11) + -1D * input[0,1]*a12 + input[0,3]*a13 + -1D * input[0,3]*a13 ;
			*/
        }  

		public double[,] Inverse44(double[,] input){


			double[,] rtn = new double[4,4];

			var A2323 = input[2,2] * input[3,3] - input[2,3] * input[3,2];
			var A1323 = input[2,1] * input[3,3] - input[2,3] * input[3,1];
			var A1223 = input[2,1] * input[3,2] - input[2,2] * input[3,1];
			var A0323 = input[2,0] * input[3,3] - input[2,3] * input[3,0];
			var A0223 = input[2,0] * input[3,2] - input[2,2] * input[3,0];
			var A0123 = input[2,0] * input[3,1] - input[2,1] * input[3,0];
			var A2313 = input[1,2] * input[3,3] - input[1,3] * input[3,2];
			var A1313 = input[1,1] * input[3,3] - input[1,3] * input[3,1];
			var A1213 = input[1,1] * input[3,2] - input[1,2] * input[3,1];
			var A2312 = input[1,2] * input[2,3] - input[1,3] * input[2,2];
			var A1312 = input[1,1] * input[2,3] - input[1,3] * input[2,1];
			var A1212 = input[1,1] * input[2,2] - input[1,2] * input[2,1];
			var A0313 = input[1,0] * input[3,3] - input[1,3] * input[3,0];
			var A0213 = input[1,0] * input[3,2] - input[1,2] * input[3,0];
			var A0312 = input[1,0] * input[2,3] - input[1,3] * input[2,0];
			var A0212 = input[1,0] * input[2,2] - input[1,2] * input[2,0];
			var A0113 = input[1,0] * input[3,1] - input[1,1] * input[3,0];
			var A0112 = input[1,0] * input[2,1] - input[1,1] * input[2,0];

			var det = input[0,0] * ( input[1,1] * A2323 - input[1,2]  * A1323 + input[1,3]  * A1223 ) - input[0,1] * ( input[1,0] * A2323 - input[1,2] * A0323 + input[1,3] * A0223 ) + input[0,2] * ( input[1,0]  * A1323 -  input[1,1] * A0323 + input[1,3]* A0123 ) - input[0,3] * ( input[1,0]  * A1223 -  input[1,1] * A0223 + input[1,2] * A0123 );

			det = 1 / det;

	
			   rtn[0,0] = det * (input[1,1] * A2323 - input[1,2] * A1323 + input[1,3] * A1223);
			   rtn[0,1] = det * - (input[0,1] * A2323 - input[0,2] * A1323 + input[0,3] * A1223);
			   rtn[0,2] = det * (input[0,1] * A2313 - input[0,2] * A1313 + input[0,3] * A1213);
			   rtn[0,3] = det * - (input[0,1] * A2312 - input[0,2] * A1312 + input[0,3] * A1212);
			   rtn[1,0] = det * - (input[1,0] * A2323 - input[1,2] * A0323 + input[1,3] * A0223);
			   rtn[1,1] = det * (input[0,0] * A2323 - input[0,2] * A0323 + input[0,3]* A0223);
			   rtn[1,2] = det * - (input[0,0] * A2313 - input[0,2] * A0313 + input[0,3] * A0213);
			   rtn[1,3] = det * (input[0,0] * A2312 - input[0,2] * A0312 + input[0,3]* A0212);
			   rtn[2,0] = det * (input[1,0] * A1323 - input[1,1] * A0323 + input[1,3] * A0123);
			   rtn[2,1] = det * - (input[0,0] * A1323 - input[0,1] * A0323 + input[0,3] * A0123);
			   rtn[2,2] = det * (input[0,0] * A1313 - input[0,1] * A0313 + input[0,3] * A0113);
			   rtn[2,3] = det * - (input[0,0] * A1312 - input[0,1] * A0312 + input[0,3] * A0112);
			   rtn[3,0] = det * - (input[1,0] * A1223 - input[1,1]* A0223 + input[1,2] * A0123);
			   rtn[3,1] = det * (input[0,0] * A1223 - input[0,1]* A0223 + input[0,2]* A0123);
			   rtn[3,2] = det * - (input[0,0] * A1213 - input[0,1] * A0213 + input[0,2] * A0113);
			   rtn[3,3] = det * (input[0,0]* A1212 - input[0,1] * A0212 + input[0,2] * A0112);

			return rtn;
		
		}
		

        protected double[,] Inverse33(double[,] input){
        
			double[,] rtn = new double[3,3];      
			double denominator = (Determinant33(input));

			rtn[0,0] = ((input[1,1] * input[2,2]) - (input[2,1] * input[1,2]))/denominator;
			rtn[0,1] = -1D*((input[1,0] * input[2,2]) - (input[2,0] * input[1,2]))/denominator;
			rtn[0,2] = ((input[1,0] * input[2,1]) - (input[2,0] * input[1,1]))/denominator;

			rtn[1,0] = -1D*((input[0,1] * input[2,2]) - (input[2,1] * input[0,2]))/denominator;
			rtn[1,1] = ((input[0,0] * input[2,2]) - (input[2,0] * input[0,2]))/denominator;
			rtn[1,2] = -1D*((input[0,0] * input[2,1]) - (input[0,1] * input[2,0]))/denominator;

			rtn[2,0] = ((input[0,1] * input[1,2]) - (input[1,1] * input[0,2]))/denominator;
			rtn[2,1] = -1D*((input[0,0] * input[1,2]) - (input[1,0] * input[0,2]))/denominator; 
			rtn[2,2] = ((input[0,0] * input[1,1]) - (input[1,0] * input[0,1]))/denominator;
			
			return rtn;
        }


		protected class KVPDoubleIntComparer3 : IComparer<double>
		{
			public int Compare(double kvp1, double kvp2)
			{
				//Check for equalities: use the same method as the R programming language
				if (kvp1 == kvp2) {
					return 1;
				}
				else {
					return (kvp2.CompareTo(kvp1));
				}
			}
		}

		protected class KVPDoubleIntComparer2 : IComparer<KeyValuePair<double, int>>
		{
			public int Compare(KeyValuePair<double, int> kvp1, KeyValuePair<double, int> kvp2)
			{
				//Check for equalities: use the same method as the R programming language
				if ((kvp1.Key == kvp2.Key) && (kvp1.Value != kvp2.Value)) {
					if (kvp1.Value > kvp2.Value) {
						return 1;
					}
					else {
						return -1;
					}
				}
				else {
					return (kvp1.Key.CompareTo(kvp2.Key));
				}
			}
		}

		[Serializable]
		protected struct Point{
			public double x;
			public double y;
			public double z;
			public double w;
			public Point(double _x, double _y, double _z, double _w){
				x = _x;
				y = _y;
				z = _z;
				w = _w;
			}
		}
		
        protected static double[]? MatrixMultiply(double[] a, int[] b)
        {
            if (a.Length != b.Length) {
                return null;
            }
            double[] Product = new double[a.Length];

            for (int row = 0; row < a.Length; row++) {
                Product[row] = a[row] * b[row];
            }
            return Product;
        }
        
        protected static double[]? MatrixMultiply(double[] a, double[] b)
        {
            if (a.Length != b.Length) {
                return null;
            }
            double[] Product = new double[a.Length];

            for (int row = 0; row < a.Length; row++) {
                Product[row] = a[row] * b[row];
            }
            return Product;
        }
        
        protected static double[,]? MatrixMultiply(double[,] a, double[] b)
        {
            if (b.Length != a.GetLength(0)) {
                return null;
            }
            double[,] Product = new double[a.GetLength(0), a.GetLength(1)];

            for (int row = 0; row < a.GetLength(0); row++) {
                for (int col = 0; col < a.GetLength(1); col++) {
                    Product[row, col] = a[row, col] * b[row];
                }
            }
            return Product;
        }
        
        public static double[,]? MatrixMultiply(double[,] a, double[,] b)
        {
            if (b.GetLength(0) != a.GetLength(0)) {
                return null;
            }
            if (b.GetLength(1) != a.GetLength(1)) {
                return null;
            }
            double[,] Product = new double[a.GetLength(0), a.GetLength(1)];

            for (int row = 0; row < a.GetLength(0); row++) {
            for (int col = 0; col < a.GetLength(1); col++) {
            	Product[row, col]=0D;
				for(int k= 0 ; k < a.GetLength(1); k++){
					Product[row, col] = Product[row, col] + (a[row, k] * b[k,col]);
                }
            }
            }
            return Product;
        }        
        
        protected double ArraySum(double[] a)
        {
            double info = 0D;
            for (int i = 0; i < a.Length; i++) {
                info = info + a[i];
            }
            return info;
        }
        protected double ArraySum(double[,] a)
        {
            double info = 0D;
            for (int i = 0; i < a.GetLength(0); i++) {
            for (int j = 0; j < a.GetLength(1); j++) {
            
            	//if(!double.IsNaN(a[i,j])){
					info = info + a[i,j];
                //}
            }
            }
            return info;
        }
        protected double ArraySum(double[,,] a)
        {
            double info = 0D;
            for (int i = 0; i < a.GetLength(0); i++) {
            for (int j = 0; j < a.GetLength(1); j++) {
            for (int k = 0; k < a.GetLength(2); k++) {
				info = info + a[i,j,k];
            }
            }
            }
            return info;
        }
        protected double ArraySum(double[,,,] a)
        {
            double info = 0D;
            for (int i = 0; i < a.GetLength(0); i++) {
            for (int j = 0; j < a.GetLength(1); j++) {
            for (int k = 0; k < a.GetLength(2); k++) {
            for (int l = 0; l < a.GetLength(2); l++) {	
				info = info + a[i,j,k,l];
            }
        	}
            }
            }
            return info;
        }

        protected double ArraySum(int[] a)
        {
            int info = 0;
            for (int i = 0; i < a.Length; i++) {
                info = info + a[i];
            }
            return (double)info;
        }        
        protected double[] MatrixReduceBySum(double[,] a)
        {
            double[] sum;
            int row;
            int col;

			sum = new double[a.GetLength(1)];
			for (col = 0; col < a.GetLength(1); col++) {
				for (row = 0; row < a.GetLength(0); row++) {
					sum[col] = sum[col] + a[row, col];
				}
			}
 
            return sum;
        }
	

	public void recordResponses(double[] theta){

		   FileStream fs = new FileStream(@"CumulativeCategory.txt", FileMode.OpenOrCreate, FileAccess.Write);
		   StreamWriter sw = new StreamWriter(fs,Encoding.UTF8);

			sw.WriteLine(" theta[1]: " + theta[0].ToString() + ", theta[2]: " + theta[1].ToString() + ", theta[3]: " + theta[2].ToString() +  ", theta[g]: " + theta[3].ToString() );

            for (int item = 0; item < _NumTotalItems; item++)
            {   
            	string? key = _Items[item]!.ToString();
            	if (key is not null){
					int selectedResponse = this.SelectResponse(key, theta, sw );
				}
			}
		    sw.Flush();
   			sw.Close();
   			fs.Close();
	
	}

	public int SelectResponse(string FormItemOID, double[] theta, StreamWriter sw )
	{
        
		
		int ItemIndex = (int)_ItemIDs[FormItemOID]!;

		Random rnd = new Random(Guid.NewGuid().GetHashCode());
		double selectedCategory =  rnd.NextDouble();
		
		int ResponseOptions = _NumCategoriesByItem[ItemIndex];
		
		int rtn =  -1;
		
		double[] Probability = new double[ ResponseOptions];
		double[] CumulativeCategory = new double [ ResponseOptions + 1];

		Probability[0] = 1D;
		CumulativeCategory[0] = 0D;
		CumulativeCategory[ResponseOptions] = 1D;

		sw.WriteLine("item: " + FormItemOID + " ResponseOptions: " + ResponseOptions.ToString() + " : random threshold: " + selectedCategory.ToString());

		for (int cat = 1; cat < ResponseOptions; cat++) //0,1,2,3
		{
			double sumvector = (_DiscriminationValues[ItemIndex,0] * theta[0] + _DiscriminationValues[ItemIndex,1] * theta[1] + _DiscriminationValues[ItemIndex,2] * theta[2] + _DiscriminationValues[ItemIndex,3] * theta[3]); 
			Probability[cat] = 1 / (1 + Math.Exp( -1 * (_LogisticScaling * sumvector + _CategoryBoundaryValues[ItemIndex][cat-1]) ) );
			CumulativeCategory[cat] = 1D - Probability[cat]; 


			//Console.WriteLine(FormItemOID + " Probability[" + cat.ToString() + "] " +  Probability[cat] .ToString()+ " CumulativeCategory[" + cat.ToString() + "] " +  CumulativeCategory[cat] .ToString() );
		}
		
		// First check the last category
		//if(selectedCategory > CumulativeCategory[ResponseOptions - 1]){
		//	sw.WriteLine("category: " + ResponseOptions.ToString() + " : cumulative category: " + CumulativeCategory[ResponseOptions - 1].ToString());
		//	rtn = ResponseOptions;
		//} else {
			for (int cat = 0; cat < ResponseOptions; cat++)
			{

				sw.WriteLine("category: " + cat.ToString() + " : cumulative category: " + CumulativeCategory[cat].ToString());

				if( (selectedCategory > CumulativeCategory[cat]) &&  (selectedCategory < CumulativeCategory[cat+1]) ){
					rtn = (cat+1);
					//break;
				}
			}

		//}		
		sw.WriteLine("Category selected:" + rtn.ToString() );	
		return rtn;
	}

	}
}
