# Movement handlers
import GameLogic
import Rasterizer as raster
import math

# Will be done once after loading the module
raster.showMouse(True)

# Constants
DELTA_X_FACTOR = 0.0005
DELTA_Y_FACTOR = -0.0005
DELTA_ROTATION_Z_FACTOR = -0.0005

CONST_DELTA_X = 0.1
CONST_DELTA_Y = -0.1

# Moving and rotating the object with a mouse
def mouseMove (controller):
    owner = controller.owner
    mouse = owner.sensors['Mouse']

    width = float(raster.getWindowWidth()) 
    height = float(raster.getWindowHeight()) 

    deltaMouseX = int((mouse.position[0] - width / 2.0)) 
    deltaMouseY = -int((mouse.position[1] - height))

    deltaRotationZ = DELTA_ROTATION_Z_FACTOR*deltaMouseX

    rotation = owner.localOrientation.to_euler()
    rotation.z += deltaRotationZ
    
    deltaX = DELTA_X_FACTOR * (math.sin(rotation.z)*deltaMouseX + math.sin(rotation.z)*deltaMouseY)
    deltaY = DELTA_Y_FACTOR * (math.cos(rotation.z)*deltaMouseX + math.cos(rotation.z)*deltaMouseY)

    owner.position = [owner.position.x + deltaX, owner.position.y + deltaY, owner.position.z]
    owner.localOrientation = rotation.to_matrix()
    
# Constant movement toward
def constantMove (controller):
    owner = controller.owner
    rotation = owner.localOrientation.to_euler()
    
    deltaX = math.sin(rotation.z) * CONST_DELTA_X
    deltaY = math.cos(rotation.z) * CONST_DELTA_Y

    owner.position = [owner.position.x + deltaX, owner.position.y + deltaY, owner.position.z]
    
    # Moving the sky
    scene = GameLogic.getCurrentScene()
    space = scene.objects['Space']
    space.worldPosition = owner.position
