package dataincubator;

import java.util.ArrayList;
import java.util.List;

public class CoinToss {
	
	// Number of coin tosses
	public int N = 100;
	
	public static final double P_HEAD = 0.6;
	public static final double P_TAIL = 0.4;
	
	public int stictlyexceedingHeads = 1;
	
	// Dynamic programming matrix
	public Block[][] dpMat = new Block[N][N];
	
	
	public void travese(List<Block> path, Params param){
		if(path.size()>0){
			Block tip = path.get(path.size()-1);
			if(!(param.h>stictlyexceedingHeads)){
				if(param.l==1){ // at head 
					if(!tip.diag && param.r>0 && param.c>0 && dpMat[param.r-1][param.c-1].TailBot > 0){ // go diag 
						tip.diag = true;
						param.p = Math.exp( Math.log(param.p) + Math.log(P_HEAD) );
						path.add(dpMat[param.r-1][param.c-1]);
						param.h++;
						param.r--;
						param.c--;
						param.l =2;
						travese(path,param);
					}else if(!tip.up && param.r>0 && param.c>0 && dpMat[param.r-1][param.c-1].headTop > 0){ // go up
						tip.up = true;
						param.p =  Math.exp( Math.log(param.p) + Math.log(P_HEAD));
						param.h++;
						path.add(dpMat[param.r-1][param.c]);
						param.r--;
						param.l = 1;
						travese(path,param);
					}else { // all paths traversed; retreat 
						tip.reset();
						path.remove(path.size()-1);
						if(path.size()==0){
							return;
						}
						int newR = path.get(path.size()-1).rowInd;
						int newC = path.get(path.size()-1).colInd;
						if(newR>param.r && newC > param.c){ //diag back
							param.l=2;
							param.p =  Math.exp( Math.log(param.p) - Math.log(P_TAIL));
						}else{ // down back
							param.l=1;
							param.p =  Math.exp( Math.log(param.p) - Math.log(P_HEAD));
							param.h--;
						}
						param.r = newR;
						param.c = newC;
						travese(path,param);
					}
				}else{
					System.out.println("here");
					if(!tip.diag && param.r>0 && param.c>0 && dpMat[param.r-1][param.c-1].headTop > 0){ // go diag
						tip.diag = true;
						param.p = Math.exp( Math.log(param.p) + Math.log(P_TAIL) );
						path.add(dpMat[param.r-1][param.c-1]);
						param.r--;
						param.c--;
						param.l =1;
						travese(path,param);
					}else if(!tip.up && param.r>0 && param.c>0 && dpMat[param.r-1][param.c-1].TailBot > 0){
						tip.up = true;
						param.p =  Math.exp( Math.log(param.p) + Math.log(P_TAIL));
						path.add(dpMat[param.r-1][param.c]);
						param.r--;
						param.l = 2;
						travese(path,param);
					}else {
						tip.reset();
						path.remove(path.size()-1);
						if(path.size()==0){
							return;
						}
						int newR = path.get(path.size()-1).rowInd;
						int newC = path.get(path.size()-1).colInd;
						if(newR>param.r && newC >param.c){ //diag back
							param.l=1;
							param.h--;
							param.p =  Math.exp( Math.log(param.p) - Math.log(P_HEAD));
						}else{ // down back
							param.l=2;
							param.p =  Math.exp( Math.log(param.p) - Math.log(P_TAIL));
						}
						param.r = newR;
						param.c = newC;
						travese(path,param);
					}

				}
			}else{ // you have all the heads you need
				tip.reset();
				path.remove(path.size()-1);
				if(path.size()==0){
					return;
				}
				int newR = path.get(path.size()-1).rowInd;
				int newC = path.get(path.size()-1).colInd;
				if(param.l==1){
					param.s=param.s+(Math.exp(Math.log(tip.headTop)+Math.log(param.p)));
					if(newR>param.r && newC > param.c){ //diag back
						param.l=2;
						param.p =  Math.exp( Math.log(param.p) - Math.log(P_TAIL));
					}else{
						param.l=1;
						param.p =  Math.exp( Math.log(param.p) - Math.log(P_HEAD));
						param.h--;
					}
					
					
				}else{
					param.s=param.s+(Math.exp(Math.log(tip.TailBot)+Math.log(param.p)));
					if(newR>param.r && newC > param.c){ //diag back
						param.l=1;
						param.p =  Math.exp( Math.log(param.p) - Math.log(P_HEAD));
						param.h--;
						
					}else{
						param.l=2;
						param.p =  Math.exp( Math.log(param.p) - Math.log(P_TAIL));
					}
				}
				param.r = newR;
				param.c = newC;
				travese(path,param);
				
			}
		}
	}
	
	
	// Gettors
	
	
	public void getStrictlyExceedingGivenHeads(int g){
		double ans = 0.0;
		for(int i=g; i<N; i++){
			List<Block> path = new ArrayList<Block>();
			//Start at head
			path.add(dpMat[N-1][i]);
			Params param = new Params();
			param.r = N-1;param.c = i;param.l = 1;param.h = 0;param.s = 0.0;param.p = 1;
			travese(path,param);
			System.out.println(param.s);
			ans = ans + param.s;
			// Now tail
			
			List<Block> pathnew = new ArrayList<Block>();
			pathnew.add(dpMat[N-1][i]);
			param = new Params();
			param.r = N-1;param.c = i;param.l = 2;param.h = 0;param.s = 0.0;param.p = 1;
			travese(pathnew,param);

			System.out.println(param.s);
			ans = ans + param.s;
		}
		System.out.println(ans);
	}
	
	public void getStrictlyExceedingGiven(int g, int given){
		double marginalizationConstat = 0.0;
		double prob = 0.0;
		for(int i=given; i<N; i++){
			marginalizationConstat = marginalizationConstat + dpMat[N-1][i].headTop + dpMat[N-1][i].TailBot;
		}
		for(int i=g; i<N; i++){
			prob = prob + dpMat[N-1][i].headTop + dpMat[N-1][i].TailBot;
		}
		System.out.println(prob/marginalizationConstat);
	}
	
	public void getStrictlyExceeding(int g){
		double prob = 0.0;
		for(int i=g; i<N; i++){
			prob = prob + dpMat[N-1][i].headTop + dpMat[N-1][i].TailBot;
		}
		System.out.println(prob);
	}
	
	public void getExpectation(){
		double exp = 0.0;
		for(int i=0; i<N; i++){
			exp = exp + (i+1)*(dpMat[N-1][i].headTop + dpMat[N-1][i].TailBot);
		}
		System.out.println(exp);
	}
	
	// Settors
	public void fillmat(){

		for(int r=0; r<N; r++){ // Over no. of tosses
			for(int c=0; c<N; c++){ // Over no. of groups
				if(c==0){ // First column is easy
					dpMat[r][c] = new Block();
					dpMat[r][c].rowInd = r;
					dpMat[r][c].colInd = c;
					dpMat[r][c].headTop = getHeadStretchProb(r+1);
					dpMat[r][c].TailBot = getTailStretchProb(r+1);
				}else if(c>r){
					dpMat[r][c] = new Block();
					dpMat[r][c].rowInd = r;
					dpMat[r][c].colInd = c;
					dpMat[r][c].headTop = 0;
					dpMat[r][c].TailBot = 0;
				}else if(r>0){
					dpMat[r][c] = new Block();
					dpMat[r][c].rowInd = r;
					dpMat[r][c].colInd = c;
					
					// first, headTop
					double currHeadTop = 0.0;
					// from top
					if(dpMat[r-1][c].headTop != 0){
						currHeadTop =  Math.exp(Math.log(dpMat[r-1][c].headTop) + Math.log(P_HEAD));
					}

					// from diag
					if(dpMat[r-1][c-1].TailBot != 0){
						currHeadTop = currHeadTop + Math.exp(Math.log(dpMat[r-1][c-1].TailBot) + Math.log(P_HEAD));
					}
					dpMat[r][c].headTop = currHeadTop;
					
					// Next, tailBot
					double currTailBot = 0.0;
					// from top
					if(dpMat[r-1][c].TailBot != 0){
						currTailBot =  Math.exp(Math.log(dpMat[r-1][c].TailBot) + Math.log(P_TAIL));
					}
					
					// from diag
					if(dpMat[r-1][c-1].headTop != 0){
						currTailBot = currTailBot + Math.exp(Math.log(dpMat[r-1][c-1].headTop) + Math.log(P_TAIL));
					}
					dpMat[r][c].TailBot = currTailBot;

				}
			}
		}
	}
	
	private double getHeadStretchProb(int l){
		double ret = 0.0;
		ret = l*Math.log(P_HEAD);
		return Math.exp(ret);
	}
	
	private double getTailStretchProb(int l){
		double ret = 0.0;
		ret = l*Math.log(P_TAIL);
		return Math.exp(ret);
	}
	
	public class Params{
		public int r = 0;
		public int c = 0;
		public int l = 0;
		public int h = 0;
		public double s = 0.0;
		public double p = 0.0;
	}
	
	public class Block{
		public int rowInd = 0; // Number of tosses
		public int colInd = 0; // Number of groups
		public boolean diag  = false; // has this node been visited
		public boolean up = false;
		
		public void reset(){
			diag = false;
			up = false;
		}
		
		
		public double headTop = 0.0;
		public double TailBot = 0.0;
		
		// Settors
		public void setRowInd(int r){rowInd = r;}
		public void setColInd(int c){colInd = c;}
		public void setHeadTop(double d){headTop = d;}
		public void setTailBot(double d){TailBot = d;}
	}
	
	public static void main(String[] args){
		CoinToss ct = new CoinToss();
		ct.fillmat();
		ct.getStrictlyExceedingGivenHeads(5);
	}

}
