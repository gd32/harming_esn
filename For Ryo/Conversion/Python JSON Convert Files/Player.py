'''
Created on 2013/06/06

@author: Hirokazu Shirado
'''

class Player:
    def __init__(self, id, gameId, cost, profit, address, isAI=False):
        self.Id = id
        self.gameId = gameId
        self.Strategy = {}
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
        self.CooperativeNeighbors = {}
        self.Payoff = {}
        self.TotalPayoff = {}
        self.cost = cost
        self.profit = profit
        self.address = address
        self.isAI = isAI
        self.Satisfaction = {}
        self.Gender = ''
        self.Age = ''
        self.Is_Hispanic = ''
        self.Race = ''

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




#gender, age, hispanic, ethnicity

    def setInitialScore(self, initialScore):
        self.initialScore = initialScore
        self.TotalPayoff[0] = initialScore


    def setIPAddress(self, address):
        self.address = address

    def setStrategy(self, round, strategy, time):
        self.Strategy[round] = strategy
        self.StrategyTime[round] = time

    def setMakeLink(self, round, targetId):
        if not self.MakeLink.has_key(round):
            self.MakeLink[round] = []
        self.MakeLink[round].append(targetId)

    def setTryMakeLink(self, round, targetId, time):
        if not self.TryMakeLink.has_key(round):
            self.TryMakeLink[round] = []
        self.TryMakeLink[round].append(targetId)

        if not self.TryMakeLinkTime.has_key(round):
            self.TryMakeLinkTime[round] = []
        self.TryMakeLinkTime[round].append(time)


    def setBreakLink(self, round, targetId, time):
        if not self.BreakLink.has_key(round):
            self.BreakLink[round] = []
        self.BreakLink[round].append(targetId)

        if not self.BreakLinkTime.has_key(round):
            self.BreakLinkTime[round] = []
        self.BreakLinkTime[round].append(time)


    def setNotMakeLink(self, round, targetId, time):
        if not self.NotMakeLink.has_key(round):
            self.NotMakeLink[round] = []
        self.NotMakeLink[round].append(targetId)

        if not self.NotMakeLinkTime.has_key(round):
            self.NotMakeLinkTime[round] = []
        self.NotMakeLinkTime[round].append(time)


    def setNotBreakLink(self, round, targetId, time):
        if not self.NotBreakLink.has_key(round):
            self.NotBreakLink[round] = []
        self.NotBreakLink[round].append(targetId)

        if not self.NotBreakLinkTime.has_key(round):
            self.NotBreakLinkTime[round] = []
        self.NotBreakLinkTime[round].append(time)


    #def setBeMadeLink(self, round, targetId):
    #    if not self.BeMadeLink.has_key(round):
    #        self.BeMadeLink[round] = []
    #    self.BeMadeLink[round].append(targetId)

    def setBeBrokenLink(self, round, targetId):
        if not self.BeBrokenLink.has_key(round):
            self.BeBrokenLink[round] = []
        self.BeBrokenLink[round].append(targetId)

    def setNeighbors(self, round, neighbors):
        self.Neighbors[round] = neighbors

    def setCooperators(self, round, cooperators):
        self.Cooperators[round] = cooperators

    def setCooperativeNeighbors(self, round, c_neighbors):
        self.CooperativeNeighbors[round] = c_neighbors

    def setPayoff(self, round):
        payoff = 0
        if round != 0: #removing initial condition
            if self.Strategy.has_key(round) and self.Neighbors.has_key(round-1): #Note: select strategy based on previous round structure
                if self.Strategy[round] == 'C':
                    payoff -= self.cost * len(self.Neighbors[round-1])
                #payoff += self.profit * c_num
                c_neighbors = [x for x in self.Neighbors[round -1] if x in self.Cooperators[round]]
                payoff += self.profit * len(c_neighbors)
            self.Payoff[round] = payoff
            if not self.TotalPayoff.has_key(round - 1):
                maxRound = max(self.TotalPayoff.keys())
                for i in range(maxRound, round):
                    self.TotalPayoff[i] = self.TotalPayoff[maxRound]
            self.TotalPayoff[round] = self.TotalPayoff[round-1] + payoff


    #Added for happiness

    def setSatisfaction(self,round,satisfaction):
        self.Satisfaction[round] = satisfaction

    def setGender(self, gender):
        self.Gender = gender

    def setAge(self,age):
        self.Age = age

    def setRace(self,race):
        self.Race = race

    def setIs_Hispanic(self,hispanic):
        self.Is_Hispanic = hispanic

    def setQ1(self, interested, distressed, excited, upset, strong):
        self.interested = interested
        self.distressed = distressed
        self.excited = excited
        self.upset = upset
        self.strong = strong


    def setQ2(self, guilty, scared, hostile, enthusiastic, proud):
        self.guilty = guilty
        self.scared = scared
        self.hostile = hostile
        self.enthusiastic = enthusiastic
        self.proud = proud

    def setQ3(self, irritable, alert, ashamed, inspired, nervous):
        self.irritable = irritable
        self.alert = alert
        self.ashamed = ashamed
        self.inspired = inspired
        self.nervous = nervous

    def setQ4(self, determined, attentive, jittery, active, afraid):
        self.determined = determined
        self.attentive = attentive
        self.jittery = jittery
        self.active = active
        self.afraid = afraid



    def removeStrategy(self, round):
        del self.Strategy[round]

    def isComplete(self, gameLength):
        if len(self.Strategy) >= gameLength:
            return True
        else:
            return False

    def getStrategy(self, round):
        if self.Strategy.has_key(round):
            return self.Strategy[round]
        return None

    def getCooperationRate(self):
        round = max(self.TotalPayoff.keys()) - 1
        s = 0
        c = 0
        for key, value in sorted(self.Strategy.items()):
            if key < round + 1:
                s += 1
                if value == 'C':
                    c += 1
        if s > 0:
            return 1.0 * c / s
        else:
            return None

    def getCooperationNum(self):
        round = max(self.TotalPayoff.keys()) - 1
        s = 0
        c = 0
        for key, value in sorted(self.Strategy.items()):
            if key < round + 1:
                s += 1
                if value == 'C':
                    c += 1
        return c, s

    def getTotalPoint(self):
        TotalPoint = [self.initialScore]
        for round, payoff in sorted(self.Payoff.items()):
            if len(TotalPoint) < round + 1:
                for x in range(round):
                    TotalPoint.append(self.initialScore)
            lastPoint = TotalPoint[-1]
            TotalPoint.append(lastPoint + payoff)
        return TotalPoint


    def getStrategyTime(self, round):
        if self.StrategyTime.has_key(round):
            return self.StrategyTime[round]
        return None;

    def getSatisfaction(self,round):
        if self.Satisfaction.has_key(round):
            return self.Satisfaction[round]
        return None;