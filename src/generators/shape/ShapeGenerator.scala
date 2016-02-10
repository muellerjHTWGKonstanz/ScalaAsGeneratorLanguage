package generators.shape


import java.nio.file._

import generators.{Resource, ProjectPropertiesMock}
import model.shapecontainer.ShapeContainerElement
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics.GeometricModel

import scala.collection.mutable.HashMap

/**
 * Created by julian on 19.01.16.
 * generates a complete shape
 */
object ShapeGenerator {

  val Default_File_Location = System.getProperty("user.home") //TODO only for testing

  val JOINTJS_PATH = "jointjs-gen/diagram"
  val JOINTJS_SHAPE_FILENAME = Default_File_Location + "shape.js"
  val JOINTJS_INSPECTOR_FILENAME = Default_File_Location + "inspector.js"
  val JOINTJS_CONNECTION_FILENAME = Default_File_Location + "connectionstyle.js"
  val JOINTJS_SHAPE_AND_INLINE_STYLE_FILENAME = Default_File_Location + "elementAndInlineStyle.js"

  def doGenerate(resource: Resource) {
    if (!resource.getPath.endsWith(ProjectPropertiesMock.SHAPES_FILE_EXTENSION)) {
      println("Shape generator is NOT producing JointJS code for model " + resource.getPath) //TODO replace with call to logger
      return
    }

    println("Shape generator is producing JointJS code for model " + resource.getPath)
    ProjectPropertiesMock.setModelUri(resource.getURI)

    //super.doGenerate(resource, fsa); TODO i dont know what super(IGenerator?) does...
    //val JavaGenFile java = genFileProvider.get()
    //java.access = fsa

    val attrs = GeneratorShapeDefinition.attrsInspector

    //---------------------------------------------------------------------------------------
    // Shapes

    //val jointJSShapeOutputConfName = "JointJSshapeOutputConfiguration"
    //  fsa.addJointJSOutputConfiguration(jointJSShapeOutputConfName) TODO dont know what this xtext element does

    val packageName = resource.getPath.getParent.toString
    var jointJSShapeContent = ""


    //Write Head of ShapeFile
    jointJSShapeContent = GeneratorShapeDefinition.head(packageName)

    //Write different Shape Definitions
    for (shapeDefinition <- resource.cache.shapeHierarchy.nodeView.values) {
      jointJSShapeContent += generateJointJSShape(shapeDefinition.data, packageName)
    }

    //Generate ShapeFile
    //fsa.generateFile(JOINTJS_SHAPE_FILENAME, jointJSShapeOutputConfName, jointJSShapeContent)
    Files.write(Paths.get(JOINTJS_SHAPE_FILENAME), jointJSShapeContent.getBytes)

    //---------------------------------------------------------------------------------------
    //ConnectionStyle
    //
    //val jointJSConnectionStyleOutputConfName = "JointJSConnectionStyleOutputConfiguration"
    //  fsa.addJointJSOutputConfiguration(jointJSConnectionStyleOutputConfName)

    var jointJsConnectionContent = ""

    //Write connection Style file
    //jointJsConnectionContent = generatorConnectionDefinition.generate(resource.allContents.toIterable.filter(typeof(ConnectionDefinition))).toString
    jointJsConnectionContent = GeneratorConnectionDefinition.generate(resource.cache.connections.values)

    //Generate Connection Style file
    //  fsa.generateFile(JOINTJS_CONNECTION_FILENAME, jointJSShapeOutputConfName, jointJsConnectionContent);
    Files.write(Paths.get(JOINTJS_CONNECTION_FILENAME), jointJsConnectionContent.getBytes)

    //---------------------------------------------------------------------------------------
    // Inspector

    //val jointJSInspectorOutputConfName = "JointJSInspectorOutputConfiguration"
    //  fsa.addJointJSOutputConfiguration(jointJSInspectorOutputConfName)

    var jointJSInspectorContent = ""

    //Write Head of Inspector File
    jointJSInspectorContent = GeneratorInspectorDefinition.head

    //Write different Inspector Definitions
    var lastElement = false

    /* Comment Markus Gerhart
       for(shapeContainerElement : resource.allContents.toIterable.filter(typeof(ShapeContainerElement))) {
         if(shapeContainerElement == resource.allContents.toIterable.filter(typeof(ShapeContainerElement)).last){
           lastElement = true;
         }

           jointJSInspectorContent = jointJSInspectorContent + java.generateJointJSInspector(shapeContainerElement, packageName, lastElement).toString()
       }
       */
    for (shapeDefinition <- resource.cache.shapeHierarchy.nodeView.values) {
      if (shapeDefinition == resource.cache.shapeHierarchy.nodeView.values.last) {
        lastElement = true
      }

      jointJSInspectorContent += generateJointJSInspector(shapeDefinition.data, packageName, lastElement, attrs)
    }

    //Write Footer of Inspector File
    jointJSInspectorContent += GeneratorInspectorDefinition.footer

    //  Generate InspectorFile
    //fsa.generateFile(JOINTJS_INSPECTOR_FILENAME, jointJSInspectorOutputConfName, jointJSInspectorContent)
    Files.write(Paths.get(JOINTJS_INSPECTOR_FILENAME), jointJSInspectorContent.getBytes)

    //---------------------------------------------------------------------------------------
    // ElementAndInlineStyle

    //val jointJSShapeAndInlineStyleOutputConfName = "JointJSElementAndInlineStyleOutputConfiguration"
    //  fsa.addJointJSOutputConfiguration(jointJSShapeAndInlineStyleOutputConfName)

    var jointJSShapeAndInlineStyleContent = ""

    //  //Write Head of Shape Style
    jointJSShapeAndInlineStyleContent = GeneratorShapeAndInlineStyle.shapeStyleHead

    lastElement = false

    //Write Shape Style for different Shape Definitions
    for(shapeDefinition <- resource.cache.shapeHierarchy.nodeView.values) {
      if(shapeDefinition == resource.cache.shapeHierarchy.nodeView.values.last){
        lastElement = true
      }
      jointJSShapeAndInlineStyleContent += generatorShapeStyle(shapeDefinition.data, packageName, lastElement, attrs)
    }

    //Write Footer of Shape Style
    jointJSShapeAndInlineStyleContent += GeneratorShapeAndInlineStyle.shapeStyleFooter

    //Generate ShapeFile
    //  fsa.generateFile(JOINTJS_SHAPE_AND_INLINE_STYLE_FILENAME, jointJSShapeAndInlineStyleOutputConfName, jointJSShapeAndInlineStyleContent)
    Files.write(Paths.get(JOINTJS_SHAPE_AND_INLINE_STYLE_FILENAME), jointJSShapeAndInlineStyleContent.getBytes)

  }

  def generateJointJSShape(shape: Shape, packageName: String) = {
    GeneratorShapeDefinition.generate(shape, packageName)
  }

  def generatorShapeStyle( shape:Shape, packageName: String, lastElement: Boolean, attrs: HashMap[String, HashMap[GeometricModel, String]] ) {
    GeneratorShapeAndInlineStyle.generateShapeStyle(shape, packageName, lastElement, attrs)
  }

  def generateJointJSInspector(shape: ShapeContainerElement, packageName: String, lastElement: Boolean, attrs: HashMap[String, HashMap[GeometricModel, String]]) = {
    GeneratorInspectorDefinition.generate(shape, packageName, lastElement, attrs)
  }


  //def private addJointJSOutputConfiguration(IFileSystemAccess fsa, String svgOutputConfName) {
  //fsa.addImageOutputConfiguration(svgOutputConfName, JOINTJS_PATH)
  //}
  //
  //def private addImageOutputConfiguration(IFileSystemAccess fsa, String outputConfName, String path) {
  //if(fsa instanceof AbstractFileSystemAccess) {
  //val aFsa = fsa as AbstractFileSystemAccess
  //if(!aFsa.outputConfigurations.containsKey(outputConfName)) {
  //val outputConfigurations = <String, OutputConfiguration> newHashMap
  //outputConfigurations.putAll(aFsa.outputConfigurations)
  //val imageOutputConfiguration = new OutputConfiguration(outputConfName)
  //imageOutputConfiguration.outputDirectory = path
  //imageOutputConfiguration.createOutputDirectory = true
  //imageOutputConfiguration.overrideExistingResources = true
  //imageOutputConfiguration.setDerivedProperty = true
  //outputConfigurations.put(outputConfName, imageOutputConfiguration)
  //aFsa.setOutputConfigurations(outputConfigurations)
  //}
  //}
  //}
}







