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
function mousemove(e){
	formatClick(e);
	if(selected!=null){
		selected.x+=e.x-mouse[0];
		selected.y+=e.y-mouse[1];
	}
	mouse=[e.x,e.y];
}
function clickCanvas(){
	var x=mouse[0];
	var y=mouse[1];
	for(var a=nodes.length-1;a>=0;a--){
		var node=nodes[a];
		if(distance(node,x,y)<=node.radius){
			Shiny.onInputChange("gene",node.name);
			Shiny.onInputChange("gene1","none");
			return;
		}
	}
	for(var a=edges.length-1;a>=0;a--){
		if(edgeClick(nodes[edges[a].start],nodes[edges[a].end],edges[a].width)){
			return;
		}
	}
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
}
function Edge(color,width,sign,start,end){
	this.color=color;
	this.width=width;
	this.sign=sign;
	this.start=start;
	this.end=end;
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
	}
}
function establishConnection(gene1,gene2,color,width,sign){
	for(var a=0;a<edges.length;a++){
		if((edges[a].start==gene1 && edges[a].end==gene2) || (edges[a].start==gene2 && edges[a].end==gene1)){
			return;
		}
	}
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
		edges.push(new Edge(color,width,sign,index1,index2));
		nodes[index1].radius+=5;
		nodes[index2].radius+=5;
	}
}

//drawing
var image=undefined;
function drawGraph(x,y){
	var img=new Image();
	image.width=650;
	image.height=400;
	img.src=image.src;
	var scale=0.75
	c.translate(x,y);
	c.scale(scale,scale);
	c.drawImage(img,0,0);
	c.scale(1/scale,1/scale);
	c.translate(-x,-y);
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
		c.strokeStyle=edge.color;
		c.lineWidth=edge.width;
		c.beginPath();
		var startX=nodes[edge.start].x;
		var startY=nodes[edge.start].y;
		var endX=nodes[edge.end].x;
		var endY=nodes[edge.end].y;
		c.moveTo(startX,startY)
		c.lineTo(endX,endY)
		c.stroke();
		c.closePath();
		c.fillStyle="black";
		c.fillText(edge.sign,((endX-startX)/2)+startX,((endY-startY)/2)+startY);
	}
}
function drawHelpButton(){
	var radius=15;
	var hover=false;
	if(mouse!=null && mouse[0]>=canvas.width-radius*2 && mouse[1]<=radius*2){
		hover=true;
	}
	if(hover){
		c.fillStyle="white";
	}else{
		c.fillStyle="maroon";
	}
	c.beginPath();
	c.arc(canvas.width-radius,radius,radius,0,Math.PI*2,true);
	c.closePath();
	c.fill();
	c.strokeStyle="black";
	c.lineWidth=1;
	c.stroke();
	if(hover){
		c.fillStyle="maroon";
	}else{
		c.fillStyle="white";
	}
	c.font="bold 12pt sans-serif";
	c.fillText("i",canvas.width-radius-3,radius+6);
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
	if(image==undefined){
		image=$("img")[0];
		return;
	}
	var data=document.getElementById("nodeData").innerHTML;
	if(nodeData!=data){
		nodeData=data;
		populateNodes();
	}
	c.clearRect(0,0,canvas.width,canvas.height);
	drawGraph(0,0);
	drawEdges();
	c.strokeStyle="black";
	c.lineWidth=1;
	drawNodes();
	drawHelpButton();
}
setCanvasDimensions();
update();