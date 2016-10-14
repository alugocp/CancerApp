//startup
var canvas=document.getElementById("canvas");
var c=canvas.getContext("2d");

//engine support
var nodes=Array();
var selected=null;
var lastmouse=null;
function formatClick(e){
	var b=canvas.getBoundingClientRect();
	e.x=e.clientX-b.left;
	e.y=e.clientY-b.top;
}
function mousedown(e){
	formatClick(e);
	for(var a=0;a<nodes.length;a++){
		if(distance(nodes[a],e.x,e.y)<Node.radius){
			selected=nodes[a];
			lastmouse=[e.x,e.y];
			return;
		}
	}
}
function mousemove(e){
	formatClick(e);
	if(selected!=null){
		selected.x+=e.x-lastmouse[0];
		selected.y+=e.y-lastmouse[1];
		lastmouse=[e.x,e.y];
	}
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
function Node(name,x,y){
	Node.radius=10;
	this.x=x;
	this.y=y;
	this.name=name;
	this.color=getColor(name);
	this.connections=new Array();
	if(arguments.length>3){
		for(var a=3;a<arguments.length;a++){
			this.connections.push(arguments[a]);
		}
	}
}

//nodes support
function getColor(name){
	var colors=["red","blue","green","orange","purple","yellow","gray"];
	return colors[Math.floor(Math.random()*colors.length)];
}
function drawNodes(){
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		c.fillStyle=node.color;
		c.beginPath();
		c.arc(node.x,node.y,Node.radius,0,2*Math.PI,true);
		c.closePath();
		c.fill();
	}
}
function drawEdges(){
	c.lineWidth=2;
	for(var a=0;a<nodes.length;a++){
		var node=nodes[a];
		for(var b=0;b<node.connections.length;b++){
			var node1=nodes[node.connections[b]];
			c.beginPath();
			c.moveTo(node.x,node.y);
			c.lineTo(node1.x,node1.y);
			c.stroke();
			c.closePath();
		}
	}
}

//initialize
function update(){
	c.clearRect(0,0,canvas.width,canvas.height);
	drawEdges();
	drawNodes();
	c.strokeRect(0,0,canvas.width,canvas.height);
	setTimeout(update,100);
}
nodes.push(new Node("hey",10,10,1,2));
nodes.push(new Node("bye",30,10,2));
nodes.push(new Node("hi",50,10));
update();