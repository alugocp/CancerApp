//startup
var canvas=document.getElementById("canvas");
var c=canvas.getContext("2d");

//engine support
var nodes=Array();
var edges=Array();
var selected=null;
var mouse=null;
function formatClick(e){
	var b=canvas.getBoundingClientRect();
	e.x=e.clientX-b.left;
	e.y=e.clientY-b.top;
}
function mousedown(e){
	formatClick(e);
	for(var a=nodes.length-1;a>=0;a--){
		if(distance(nodes[a],e.x,e.y)<nodes[a].radius){
			selected=nodes[a];
			return;
		}
	}
}
var moved=false;
function mousemove(e){
	formatClick(e);
	if(selected!=null){
		selected.x+=e.x-mouse[0];
		selected.y+=e.y-mouse[1];
	        moved=true;
	}
	mouse=[e.x,e.y];
}
var last;
function clickCanvas(){
	var x=mouse[0];
	var y=mouse[1];
        if(moved){
	    moved=false;
	    return;
	}
        if(help.active){
	        help.active=false;
	        help.hover=false;
	        return;
	}
        if(help.hover){
	        drawHelpButton();
	        help.active=true;
	        help.hover=false;
	        return;
	}
	for(var a=nodes.length-1;a>=0;a--){
		var node=nodes[a];
		if(distance(node,x,y)<=node.radius){
			Shiny.onInputChange("gene",node.name);
			Shiny.onInputChange("gene1","none");
		        graph.x=x;
		        graph.y=y;
		        graph.visible=true;
		        if(last!=node){
			    last=node;
			    graph.image.src=Graph.defaultSrc;
			}
			return;
		}
	}
	for(var a=edges.length-1;a>=0;a--){
		if(edgeClick(nodes[edges[a].start],nodes[edges[a].end],edges[a].width)){
		        graph.x=x;
		        graph.y=y;
		        graph.visible=true;
		        if(last!=edges[a]){
			    last=edges[a];
			    graph.image.src=Graph.defaultSrc;
			}
			return;
		}
	}
        graph.visible=false;
}
function edgeClick(node,node1,width){
	var x=mouse[0];
	var y=mouse[1];
	if((x>node.x)!=(x>node1.x) || (node.x==node1.x)){
		var m=(node.y-node1.y)/(node.x-node1.x);
		if(Math.abs(m)==Infinity){
			if((y>node.y)!=(y>node1.y)){
				if(Math.abs(x-node.x)<width){
					Shiny.onInputChange("gene",node.name);
					Shiny.onInputChange("gene1",node1.name);
					return true;
				}
			}
			return false;
		}else{
			var expectedY;
			if(node.x<node1.x){
				expectedY=(m*(x-node.x))+node.y;
			}else{
				expectedY=(m*(x-node1.x))+node1.y;
			}
			var leniency=Math.abs(width/Math.cos(Math.atan(m)));
			if(Math.abs(y-expectedY)<leniency){
				Shiny.onInputChange("gene",node.name);
				Shiny.onInputChange("gene1",node1.name);
				return true;
			}
		}
	}
	return false;
}
function distance(){
	function dist(x,y,x1,y1){
		return Math.sqrt(Math.pow(x-x1,2)+Math.pow(y-y1,2));
	}
	if(arguments.length==4){
		return dist(arguments[0],arguments[1],arguments[2],arguments[3]);
	}
	if(arguments.length==3){
		return dist(arguments[0].x,arguments[0].y,arguments[1],arguments[2]);
	}
	if(arguments.length==2){
		return dist(arguments[0].x,arguments[0].y,arguments[1].x,arguments[1].y);
	}
}

//class definitions
function Node(name,color){
	this.radius=15;
	this.name=name;
	this.color=getColor(name);
	this.x=0;
	this.y=0;
	this.connections=0;
}
function Edge(color,width,sign,start,end){
	this.color=color;
	this.width=width;
	this.sign=sign;
	this.start=start;
	this.end=end;
}
function Help(radius){
        this.x=canvas.width-radius;
        this.y=radius;
        this.left=this.x-radius;
        this.bottom=this.y+radius;
        this.radius=radius;
        this.hover=false;
        this.active=false;
}
function Graph(){
        this.image;
        this.visible=false;
        this.x=0;
        this.y=0;
        Graph.defaultSrc;//="default";
}

//nodes support
function getColor(name){
	var chars=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9"];
	var hex=["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"];
	var color="";
	var i=0;
	while(color.length<6){
		if(i>=name.length){
			i=0;
		}
		color+=hex[chars.indexOf(name.substring(i,i+1).toLowerCase())%hex.length];
		i++;
	}
	return "#"+color;
}
function setCoordinates(node,index,length){
	var angle=index*(Math.PI*2)/length;
	node.x=Math.round(650+(100*Math.cos(angle)));
	node.y=Math.round(170+(100*Math.sin(angle)));
}
var nodeData="";
function populateNodes(){
	c.font="10pt sans-serif";
	while(nodes.length>0){
		nodes.splice(0,1);
	}
	while(edges.length>0){
		edges.splice(0,1);
	}
	var data=nodeData;
	for(var a=0;a<data.length;a++){
		if(data.substring(a,a+1)=="\"" || data.substring(a,a+1)=="\n" || data.substring(a,a+1)==" "){
			data=data.substring(0,a)+data.substring(a+1);
			a--;
		}
	}
	if(data.substring(0,6)=="[1]NA,"){
		return;
	}

	information=data.split("[1]");
	information.splice(0,1);
	for(var a=0;a<information.length;a++){
		information[a]=information[a].split(",");
		var add=true;
		for(var b=0;b<nodes.length;b++){
			if(nodes[b].name==information[a][0]){
				add=false;
				break;
			}
		}
		if(add==true){
			nodes.push(new Node(information[a][0],information[a][1]));
		}
	}
	for(var a=0;a<information.length;a++){
		for(var b=3;b<information[a].length;b+=4){
			establishConnection(information[a][0],information[a][b],information[a][b+1],information[a][b+2],information[a][b+3]);
		}
	}
	for(var a=0;a<nodes.length;a++){
		setCoordinates(nodes[a],a,nodes.length);
		if(nodes[a].connections>0){
			nodes[a].radius+=Math.round(10*Math.log(nodes[a].connections));
		}
	}
}
function establishConnection(gene1,gene2,color,width,sign){
	var index1=-1;
	var index2=-1;
	for(var a=0;a<nodes.length;a++){
		if(nodes[a].name==gene1){
			index1=a;
			break;
		}
	}
	for(var a=0;a<nodes.length;a++){
		if(nodes[a].name==gene2){
			index2=a;
			break;
		}
	}
	if(index1>=0 && index2>=0){
		for(var a=0;a<edges.length;a++){
			if((edges[a].start==index1 && edges[a].end==index2) || (edges[a].start==index2 && edges[a].end==index1)){
				return;
			}
		}
		edges.push(new Edge(color,width,sign,index1,index2));
		nodes[index1].connections++;
		nodes[index2].connections++;
	}
}

//drawing
function drawGraph(){
        if(graph.visible){
	    c.translate(graph.x,graph.y);
	    if(graph.image.src==Graph.defaultSrc){
		drawLoading();
	    }else{
		var scale=0.75;
		c.scale(scale,scale);
		var offsetX=0;
		if(graph.x+(scale*graph.image.width)>canvas.width){
		    offsetX=canvas.width-(graph.x+graph.image.width);
		}
		var offsetY=0;
		if(graph.y+(scale*graph.image.height)>canvas.height){
		    offsetY=canvas.height-(graph.y+graph.image.height);
		}
   		c.drawImage(graph.image,offsetX,offsetY);
		c.scale(1/scale,1/scale);
	    }
	    c.translate(-graph.x,-graph.y);
	}
}
function drawLoading(){
    c.fillStyle="maroon";
    c.font="bold 20pt sans-serif";
    c.fillText("Loading...",0,0);
}
function drawNodes(){
	c.font="10pt sans-serif";
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		c.fillStyle=node.color;
		c.beginPath();
		c.arc(node.x,node.y,node.radius,0,2*Math.PI,true);
		c.closePath();
		c.fill();
		c.stroke();
		c.fillStyle="white";
		c.globalAlpha=0.25;
		c.beginPath();
		c.arc(node.x-(node.radius*Math.cos(Math.PI/4)/2),node.y-(node.radius*Math.sin(Math.PI/4)/2),node.radius/2,0,2*Math.PI,true);
		c.closePath();
		c.fill();
		c.globalAlpha=1;
		c.fillStyle="black";
		var delta=c.measureText(node.name).width/2;
		c.fillText(node.name,node.x-delta,node.y+5);
	}
}
function drawEdges(){
	for(var a=0;a<edges.length;a++){
		var edge=edges[a];
		var startX=nodes[edge.start].x;
		var startY=nodes[edge.start].y;
		var endX=nodes[edge.end].x;
		var endY=nodes[edge.end].y;
		var theta=Math.atan2(endY-startY,endX-startX);
		startX+=(nodes[edge.start].radius+parseInt(edge.width))*Math.cos(theta);
		startY+=(nodes[edge.start].radius+parseInt(edge.width))*Math.sin(theta);
		theta+=Math.PI;
		endX+=nodes[edge.end].radius*Math.cos(theta);
		endY+=nodes[edge.end].radius*Math.sin(theta);
		c.strokeStyle=edge.color;
		c.lineWidth=edge.width;
		c.beginPath();
		c.moveTo(startX,startY)
		c.lineTo(endX,endY)
		c.stroke();
		c.closePath();
		drawArrow(startX,startY,edge.width,edge.color,theta+Math.PI);
		c.fillStyle="black";
		c.fillText(edge.sign,((endX-startX)/2)+startX,((endY-startY)/2)+startY);
	}
}
function drawArrow(x,y,e,color,theta){
	c.fillStyle=color;
	c.translate(x,y);
	c.rotate(theta);
	c.beginPath();
	c.moveTo(-e,0);
	c.lineTo(0,e);
	c.lineTo(0,-e);
	c.lineTo(-e,0);
	c.fill();
	c.closePath();
	c.rotate(-theta);
	c.translate(-x,-y);
}
var help=new Help(15);
function drawHelpButton(){
        if(canvas.width-(help.radius*2)>help.left){
	        help.x=canvas.width-help.radius;
	        help.left=help.x-help.radius;
	}
        help.hover=false;
	if(mouse!=null && mouse[0]>=help.left && mouse[1]<=help.bottom){
		help.hover=true;
	}
	if(help.hover){
		c.fillStyle="white";
	}else{
		c.fillStyle="maroon";
	}
	c.beginPath();
	c.arc(help.x,help.y,help.radius,0,Math.PI*2,true);
	c.closePath();
	c.fill();
	c.strokeStyle="black";
	c.lineWidth=1;
	c.stroke();
	if(help.hover){
		c.fillStyle="maroon";
	}else{
		c.fillStyle="white";
	}
	c.font="bold 12pt sans-serif";
	c.fillText("i",help.x-3,help.y+6);
}
function drawHelpScreen(){
        c.fillText("Welcome to the help screen!",25,10);
        c.fillText(" Type in a single gene's name, or a list of genes separated by commas to",5,30);
        c.fillText("start exploring gene networks. Click on a gene node or a connection",5,50);
        c.fillText("between genes to view a statistical representation of how it affects",5,70);
        c.fillText("Cancer survival. All data will be graphed using Kaplan-Meier (KM) plots.",5,90);
        c.fillText(" Connections can be different colors and widths, and they all have an",5,130);
        c.fillText("arrow and either a + or - on them. Color indicates which gene expression",5,150);
        c.fillText("combination (refer to graph legend) plays a critical role in Cancer. Width",5,170);
        c.fillText("refers to our confidence in that role (how strongly we believe in it).",5,190);
        c.fillText("The + and - indicate whether the factor positively or negatively affects",5,210);
        c.fillText("Cancer survival. The arrow helps identify which gene is Gene 1. Gene 1 in",5,230);
        c.fillText("an interaction corresponds to the first expression in each combination",5,250);
        c.fillText("pair.",5,270);
}
function drawLegend(){
    c.fillStyle="maroon";
    c.font="15pt sans-serif";
    var y=canvas.height-100;
    if(mouse[1]<y){
	c.globalAlpha=0.25;
    }
    c.translate(5,y);
    c.fillText("Connection color shows what expression pair (see graph) causes an interesting interaction",0,0)
    c.fillText("Arrows point to first gene in a pair",0,20)
    c.fillText("Connection width is based on confidence in an interaction's effect",0,40)
    c.fillText("+ or - signifies positive/negative effect",0,60)
    c.fillText("Gene radius corresponds to how interconnected it is",0,80);
    c.translate(-5,-y);
    c.globalAlpha=1;
}

//initialize
function setCanvasDimensions(){
	$("body")[0].style.overflow="hidden";
	var rect=canvas.getBoundingClientRect();
	canvas.width=window.innerWidth-(rect.left*2);
	canvas.height=window.innerHeight-rect.top-rect.left;
}
function update(){
	setTimeout(update,250);
	if(graph.image==undefined){
		graph.image=$("img")[0];
	        if(graph.image!=undefined){
	            Graph.defaultSrc=graph.image.src;
		}
		return;
	}
	var data=document.getElementById("nodeData").innerHTML;
	if(nodeData!=data){
		nodeData=data;
		populateNodes();
	}
	if(help.active){
	        var width=750;
	        var x=(canvas.width-width)/2;
	        c.clearRect(x,0,width,canvas.height);
	        c.strokeStyle="black";
	        c.strokeRect(x,0,width,canvas.height);
	        c.fillStyle="maroon";
	        c.font="15pt sans-serif";
	        c.translate(x,15);
	        drawHelpScreen();
	        c.translate(-x,-15);
	}else{
		c.clearRect(0,0,canvas.width,canvas.height);
	        drawEdges();
	        drawLegend();
         	c.strokeStyle="black";
        	c.lineWidth=1;
        	drawNodes();
	        drawGraph();
        	drawHelpButton();
	}
}

var graph=new Graph();
setCanvasDimensions();
update();
