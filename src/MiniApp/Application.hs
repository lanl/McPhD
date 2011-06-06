-- | A module for building a specific particle transport mini-application
module Application where

import Mesh.Classes

import MonteCarlo

import MiniApp.Events as Events
import MiniApp.Model as Model
import MiniApp.Particle

-- Create a model

-- Get some particles, somehow. Read a file for testing?
particles :: [Particle]
particles = []


events = map (stream Model.step Events.isFinalEvent) particles
