import GameLogic
import time

collided = False

def collide(controller):
    global collided
    if not collided:
        collided = True
        time.sleep(3)
        print("Collision!")
        
        owner = controller.owner
        owner.suspendDynamics()
        
        sceneActuator = controller.actuators['GameOverScene']
        sceneActuator.scene = 'GameOver'
    controller.activate(sceneActuator)
    
    

