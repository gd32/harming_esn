'''
Created on 2013/06/06

@author: Hirokazu Shirado

Editted on 2022 by Hiroyasu Ando
'''

from datetime import datetime as dt

class Player:

    def __init__(self, id, gameId, cost, profit, address, isAI=False):

        self.Id = id
        self.gameId = gameId
        self.Strategy = {}

        # Added
        self.timeUpRound = {}

        # Added
        self.CooperationTime = {}

        self.StrategyTime = {}
        self.MakeLink = {}
        self.TryMakeLink = {}
        self.NotMakeLink = {}
        self.BreakLink = {}
        self.NotBreakLink = {}
        self.TryMakeLinkTime = {}
        self.NotMakeLinkTime = {}
        self.BreakLinkTime = {}
        self.NotBreakLinkTime = {}

        self.BeBrokenLink = {}
        self.Neighbors = {}
        self.Cooperators = {}

        # Added
        self.Punishers = {}

        self.CooperativeNeighbors = {}
        self.Payoff = {}
        self.TotalPayoff = {}
        self.cost = cost
        self.profit = profit
        self.address = address
        self.isAI = isAI
        self.Satisfaction = {}

        # Added
        self.RewiringSatisfaction = {}

        # Added
        self.InitialSatisfaction = ''

        self.Gender = ''
        self.Age = ''
        self.Is_Hispanic = ''
        self.Race = ''

        # Added
        self.hisp = ''

        self.interested = ''
        self.distressed = ''
        self.excited = ''
        self.upset = ''
        self.strong = ''

        self.guilty = ''
        self.scared = ''
        self.hostile = ''
        self.enthusiastic = ''
        self.proud = ''

        self.irritable = ''
        self.alert = ''
        self.ashamed = ''
        self.inspired = ''
        self.nervous = ''

        self.determined = ''
        self.attentive = ''
        self.jittery = ''
        self.active = ''
        self.afraid = ''

        # Added
        self.q1_time = ""
        self.q2_time = ""
        self.q3_time = ""
        self.q4_time = ""
        self.gender_time = ""
        self.age_time = ""
        self.race_time = ""
        self.hisp_time = ""
    
    def setInitialScore(self, initialScore):
        self.initialScore = initialScore

        # Initial Score is total pay off in 0 round.
        self.TotalPayoff[0] = initialScore
    
    # Added
    def setCooperationTime(self, round, time):
        self.CooperationTime[round] = time
    
    def getStrategy(self, round):
        if round in self.Strategy:
            return self.Strategy[round]
        return None
    
    def setNeighbors(self, round, neighbors):
        self.Neighbors[round] = neighbors
    
    def setCooperators(self, round, cooperators):
        self.Cooperators[round] = cooperators
    
    # Added
    def setPunishers(self, round, punishers):
        self.Punishers[round] = punishers

    def setPayoff(self, round):

        payoff = 0

        if round != 0:
            if round in self.Strategy and (round-1) in self.Neighbors:
                if self.Strategy[round] == 'C':
                    payoff -= self.cost * len(list(self.Neighbors[round-1]))
                # Added
                elif self.Strategy[round] == 'P':
                    payoff -= self.cost * len(list(self.Neighbors[round-1]))
                
                # Find c_neighbors from previous neighbors.

                c_neighbors = [x for x in self.Neighbors[round-1] if x in self.Cooperators[round]]

                # Added
                p_neighbors = [x for x in self.Neighbors[round-1] if x in self.Punishers[round]]

                payoff += self.profit * len(c_neighbors)

                # Added
                payoff -= self.profit * len(p_neighbors)

            self.Payoff[round] = payoff

            if (round - 1) not in self.TotalPayoff:
                maxRound = max(self.TotalPayoff.keys())
                for i in range(maxRound, round):
                    self.TotalPayoff[i] = self.TotalPayoff[maxRound]
            self.TotalPayoff[round] = self.TotalPayoff[round-1] + payoff
    
    def setMakeLink(self, round, targetId):
        if round not in self.MakeLink:
            self.MakeLink[round] = []
        self.MakeLink[round].append(targetId)
    
    def setStrategy(self, round, strategy, time):
        self.Strategy[round] = strategy
        #self.StrategyTime[round] = time
        
        # Added
        self.StrategyTime[round] = time - self.CooperationTime[round]
    
    # Added
    def setTimeUpStrategy(self, round, strategy):
        self.Strategy[round] = strategy
        self.timeUpRound[round] = strategy
    
    def setSatisfaction(self, round, satisfaction):
        self.Satisfaction[round] = satisfaction
    
    # Added
    def setRewiringSatisfaction(self, round, satisfaction):
        self.RewiringSatisfaction[round] = satisfaction
    
    # Added
    def setInitialSatisfaction(self, round, satisfaction):
        self.InitialSatisfaction = satisfaction
    
    def setQ1(self, interested, distressed, excited, upset, strong, time):
        self.interested = interested
        self.distressed = distressed
        self.excited = excited
        self.upset = upset
        self.strong = strong
        self.q1_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setQ2(self, guilty, scared, hostile, enthusiastic, proud, time):
        self.guilty = guilty
        self.scared = scared
        self.hostile = hostile
        self.enthusiastic = enthusiastic
        self.proud = proud
        self.q2_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setQ3(self, irritable, alert, ashamed, inspired, nervous, time):
        self.irritable = irritable
        self.alert = alert
        self.ashamed = ashamed
        self.inspired = inspired
        self.nervous = nervous
        self.q3_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setQ4(self, determined, attentive, jittery, active, afraid, time):
        self.determined = determined
        self.attentive = attentive
        self.jittery = jittery
        self.active = active
        self.afraid = afraid
        self.q4_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setGender(self, gender, time):
        self.Gender = gender
        self.gender_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setAge(self,age, time):
        self.Age = age
        self.age_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setRace(self,race, time):
        self.Race = race
        self.race_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    # Added
    def setHispanic(self, hisp, time):
        self.hisp = hisp
        self.hisp_time = dt.strftime(time, '%Y-%m-%d %H:%M:%S.%f')
    
    def setBreakLink(self, round, targetId, time):
        if round not in self.BreakLink:
            self.BreakLink[round] = []
        self.BreakLink[round].append(targetId)
        if round not in self.BreakLinkTime:
            self.BreakLinkTime[round] = []
        self.BreakLinkTime[round].append(time)
    
    def setBeBrokenLink(self, round, targetId):
        if round not in self.BeBrokenLink:
            self.BeBrokenLink[round] = []
        self.BeBrokenLink[round].append(targetId)
    
    def setTryMakeLink(self, round, targetId, time):
        if round not in self.TryMakeLink:
            self.TryMakeLink[round] = []
        self.TryMakeLink[round].append(targetId)

        if round not in self.TryMakeLinkTime:
            self.TryMakeLinkTime[round] = []
        self.TryMakeLinkTime[round].append(time)

    def setNotMakeLink(self, round, targetId, time):
        if round not in self.NotMakeLink:
            self.NotMakeLink[round] = []
        self.NotMakeLink[round].append(targetId)

        if round not in self.NotMakeLinkTime:
            self.NotMakeLinkTime[round] = []
        self.NotMakeLinkTime[round].append(time)
    
    def setNotBreakLink(self, round, targetId, time):
        if round not in self.NotBreakLink:
            self.NotBreakLink[round] = []
        self.NotBreakLink[round].append(targetId)

        if round not in self.NotBreakLinkTime:
            self.NotBreakLinkTime[round] = []
        self.NotBreakLinkTime[round].append(time)




        









            


                










        





    














































