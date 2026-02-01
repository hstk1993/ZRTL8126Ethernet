//
//  ZRTL8126EthernetSetup.cpp
//  ZRTL8126Ethernet
//
//  Created by hstk on 2026/1/27.
//

#include "ZRTL8126Ethernet.hpp"

static const char *onName = "enabled";
static const char *offName = "disabled";

#pragma mark--- data structure initialization methods ---

void ZRTL8126::getParams()
{
    OSDictionary *params;
    OSNumber *pollInt;
    OSBoolean *enableEEE;
    OSBoolean *tso4;
    OSBoolean *tso6;
    OSBoolean *csoV6;
    OSBoolean *noASPM;
    OSString *versionString;
    OSString *fbAddr;
    UInt32 usInterval;

    versionString = OSDynamicCast(OSString, getProperty(kDriverVersionName));

    params = OSDynamicCast(OSDictionary, getProperty(kParamName));

    if (params)
    {
        noASPM = OSDynamicCast(OSBoolean, params->getObject(kDisableASPMName));
        linuxData.configASPM = (noASPM) ? !(noASPM->getValue()) : 0;

        DebugLog("ZRTL8126: PCIe ASPM support %s.\n", linuxData.configASPM ? onName : offName);

        enableEEE = OSDynamicCast(OSBoolean, params->getObject(kEnableEeeName));

        if (enableEEE)
            linuxData.eee_enabled = (enableEEE->getValue()) ? 1 : 0;
        else
            linuxData.eee_enabled = 0;

        IOLog("ZRTL8126: EEE support %s.\n", linuxData.eee_enabled ? onName : offName);

        tso4 = OSDynamicCast(OSBoolean, params->getObject(kEnableTSO4Name));
        enableTSO4 = (tso4) ? tso4->getValue() : false;

        IOLog("ZRTL8126: TCP/IPv4 segmentation offload %s.\n", enableTSO4 ? onName : offName);

        tso6 = OSDynamicCast(OSBoolean, params->getObject(kEnableTSO6Name));
        enableTSO6 = (tso6) ? tso6->getValue() : false;

        IOLog("ZRTL8126: TCP/IPv6 segmentation offload %s.\n", enableTSO6 ? onName : offName);

        csoV6 = OSDynamicCast(OSBoolean, params->getObject(kEnableCSO6Name));
        enableCSO6 = (csoV6) ? csoV6->getValue() : false;

        IOLog("ZRTL8126: TCP/IPv6 checksum offload %s.\n", enableCSO6 ? onName : offName);

        pollInt = OSDynamicCast(OSNumber, params->getObject(kPollInt2500Name));

        if (pollInt)
        {
            usInterval = pollInt->unsigned32BitValue();

            if (usInterval > 150)
                pollInterval2500 = 150000;
            else if (usInterval < 75)
                pollInterval2500 = 75000;
            else
                pollInterval2500 = usInterval * 1000;
        }
        else
        {
            pollInterval2500 = 110000;
        }
        fbAddr = OSDynamicCast(OSString, params->getObject(kFallbackName));

        if (fbAddr)
        {
            const char *s = fbAddr->getCStringNoCopy();
            UInt8 *addr = fallBackMacAddr.bytes;

            if (fbAddr->getLength())
            {
                sscanf(s, "%2hhx:%2hhx:%2hhx:%2hhx:%2hhx:%2hhx", &addr[0], &addr[1], &addr[2], &addr[3], &addr[4], &addr[5]);

                IOLog("ZRTL8126: Fallback MAC: %2.2x:%2.2x:%2.2x:%2.2x:%2.2x:%2.2x\n",
                      fallBackMacAddr.bytes[0], fallBackMacAddr.bytes[1],
                      fallBackMacAddr.bytes[2], fallBackMacAddr.bytes[3],
                      fallBackMacAddr.bytes[4], fallBackMacAddr.bytes[5]);
            }
        }
    }
    else
    {
        enableTSO4 = true;
        enableTSO6 = true;
        pollInterval2500 = 0;
    }
    if (versionString)
        IOLog("ZRTL8126: ZRTL8126ETH version %s starting.\n", versionString->getCStringNoCopy());
    else
        IOLog("ZRTL8126: ZRTL8126ETH starting.\n");
}

static IOMediumType mediumTypeArray[MEDIUM_INDEX_COUNT] = {
    kIOMediumEthernetAuto,
    (kIOMediumEthernet10BaseT | kIOMediumOptionHalfDuplex),
    (kIOMediumEthernet10BaseT | kIOMediumOptionFullDuplex),
    (kIOMediumEthernet100BaseTX | kIOMediumOptionHalfDuplex),
    (kIOMediumEthernet100BaseTX | kIOMediumOptionFullDuplex),
    (kIOMediumEthernet100BaseTX | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl),
    (kIOMediumEthernet1000BaseT | kIOMediumOptionFullDuplex),
    (kIOMediumEthernet1000BaseT | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl),
    (kIOMediumEthernet100BaseTX | kIOMediumOptionFullDuplex | kIOMediumOptionEEE),
    (kIOMediumEthernet100BaseTX | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl | kIOMediumOptionEEE),
    (kIOMediumEthernet1000BaseT | kIOMediumOptionFullDuplex | kIOMediumOptionEEE),
    (kIOMediumEthernet1000BaseT | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl | kIOMediumOptionEEE),
    (kIOMediumEthernet2500BaseT | kIOMediumOptionFullDuplex),
    (kIOMediumEthernet2500BaseT | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl),
    (kIOMediumEthernet5000BaseT | kIOMediumOptionFullDuplex),
    (kIOMediumEthernet5000BaseT | kIOMediumOptionFullDuplex | kIOMediumOptionFlowControl)};

static UInt64 mediumSpeedArray[MEDIUM_INDEX_COUNT] = {
    0,
    10 * MBit,
    10 * MBit,
    100 * MBit,
    100 * MBit,
    100 * MBit,
    1000 * MBit,
    1000 * MBit,
    100 * MBit,
    100 * MBit,
    1000 * MBit,
    1000 * MBit,
    2500 * MBit,
    2500 * MBit,
    5000 * MBit,
    5000 * MBit,
};

bool ZRTL8126::setupMediumDict()
{
    IONetworkMedium *medium;
    UInt32 i;
    bool result = false;

    mediumDict = OSDictionary::withCapacity(MEDIUM_INDEX_COUNT + 1);

    if (mediumDict)
    {
        for (i = MEDIUM_INDEX_AUTO; i < MEDIUM_INDEX_COUNT; i++)
        {
            medium = IONetworkMedium::medium(mediumTypeArray[i], mediumSpeedArray[i], 0, i);

            if (!medium)
                goto error1;

            result = IONetworkMedium::addMedium(mediumDict, medium);
            medium->release();

            if (!result)
                goto error1;

            mediumTable[i] = medium;
        }
    }
    result = publishMediumDictionary(mediumDict);
    IOLog("ZRTL8126: setupMediumDict======:\n");

    if (!result)
        goto error1;

done:
    return result;

error1:
    IOLog("ZRTL8126: Error creating medium dictionary.\n");
    mediumDict->release();

    for (i = MEDIUM_INDEX_AUTO; i < MEDIUM_INDEX_COUNT; i++)
        mediumTable[i] = NULL;

    goto done;
}

bool ZRTL8126::initEventSources(IOService *provider)
{
    IOReturn intrResult;
    int numActiveInterrupts = 0;
    int intrIndex = 0;
    int intrType = 0;
    bool result = false;

    txQueue = reinterpret_cast<IOBasicOutputQueue *>(getOutputQueue());

    IOLog("ZRTL8126: initEventSources:\n");

    if (txQueue == NULL)
    {
        IOLog("ZRTL8126: Failed to get output queue.\n");
        goto done;
    }
    txQueue->retain();

    while ((intrResult = pciDevice->getInterruptType(intrIndex, &intrType)) == kIOReturnSuccess)
    {
        if (intrType & kIOInterruptTypePCIMessaged)
        {
            if (numActiveInterrupts >=R8126_MIN_MSIX_VEC_8125D)
            {
                break;
            }
            IOInterruptEventSource *source = IOInterruptEventSource::interruptEventSource(
                this,
                OSMemberFunctionCast(IOInterruptEventSource::Action, this, &ZRTL8126::interruptHandler),
                provider,
                intrIndex);

            if (source)
            {
                if (workLoop->addEventSource(source) == kIOReturnSuccess)
                {
                    interruptSources[intrIndex] = source;
                    numActiveInterrupts++;
                }
                else
                {
                    source->release();
                }
            }
        }

        intrIndex++;
    }
    if (numActiveInterrupts == 0)
    {
        IOLog("ZRTL8126: Error: MSI index was not found or MSI interrupt could not be enabled.\n");
        goto error1;
    }

    timerSource = IOTimerEventSource::timerEventSource(this, OSMemberFunctionCast(IOTimerEventSource::Action, this, &ZRTL8126::timerActionRTL8126));

    if (!timerSource)
    {
        IOLog("ZRTL8126: Failed to create IOTimerEventSource.\n");
        goto error2;
    }
    workLoop->addEventSource(timerSource);

    result = true;

done:
    return result;
error2:
    cleanupEventSources();

error1:
    IOLog("ZRTL8126: Error initializing event sources.\n");
    txQueue->release();
    txQueue = NULL;
    goto done;
}

void ZRTL8126::cleanupEventSources()
{
    if (!workLoop)
        return;
    for (int i = 0; i < R8126_MIN_MSIX_VEC_8125D; i++)
    {
        if (interruptSources[i])
        {
            workLoop->removeEventSource(interruptSources[i]);
            RELEASE(interruptSources[i]);
        }
    }
}

bool ZRTL8126::setupRxResources()
{
    IOPhysicalSegment rxSegment;
    IODMACommand::Segment64 seg;
    mbuf_t m;
    UInt32 i, j;
    UInt32 opts1;
    bool result = false;
    struct rtl8126_private *tp = &linuxData;
    struct rtl8126_rx_ring *ring;

    //    tp->HwSuppNumTxQueues = 2;
    //    tp->HwSuppNumRxQueues = 4;

    for (j = 0; j < tp->HwSuppNumRxQueues; j++)
    {
        UInt64 offset = 0;
        UInt32 numSegs = 1;
        ring = &rx_ring[j];

        /* Alloc rx mbuf_t array.todo */
        ring->rxBufArrayMem = IOMallocZero(kRxBufArraySize);

        if (!ring->rxBufArrayMem)
        {
            IOLog("ZRTL8126: Couldn't alloc receive buffer array.\n");
            goto done;
        }
        ring->rxMbufArray = (mbuf_t *)ring->rxBufArrayMem;
        ring->RxDescAllocSize = ring->num_rx_desc * tp->RxDescLength;
        /* Create receiver descriptor array. */
        ring->rxBufDesc = IOBufferMemoryDescriptor::inTaskWithPhysicalMask(kernel_task, (kIODirectionInOut | kIOMemoryPhysicallyContiguous | kIOMemoryHostPhysicallyContiguous | kIOMapInhibitCache), ring->RxDescAllocSize, 0xFFFFFFFFFFFFFF00ULL);

        if (!ring->rxBufDesc)
        {
            IOLog("ZRTL8126: Couldn't alloc rxBufDesc.\n");
            goto error_buff;
        }
        if (ring->rxBufDesc->prepare() != kIOReturnSuccess)
        {
            IOLog("ZRTL8126: rxBufDesc->prepare() failed.\n");
            goto error_prep;
        }
        ring->rxDescArray = (RtlRxDesc *)ring->rxBufDesc->getBytesNoCopy();

        ring->rxDescDmaCmd = IODMACommand::withSpecification(kIODMACommandOutputHost64, 64, 0, IODMACommand::kMapped, 0, 1, mapper, NULL);

        if (!ring->rxDescDmaCmd)
        {
            IOLog("ZRTL8126: Couldn't alloc rxDescDmaCmd.\n");
            goto error_dma;
        }

        if (ring->rxDescDmaCmd->setMemoryDescriptor(ring->rxBufDesc) != kIOReturnSuccess)
        {
            IOLog("ZRTL8126: setMemoryDescriptor() failed.\n");
            goto error_set_desc;
        }

        if (ring->rxDescDmaCmd->gen64IOVMSegments(&offset, &seg, &numSegs) != kIOReturnSuccess)
        {
            IOLog("ZRTL8126: gen64IOVMSegments() failed.\n");
            goto error_segm;
        }
        /* And the rx ring's physical address too. */
        ring->rxPhyAddr = seg.fIOVMAddr;

        /* Initialize rxDescArray. */
        bzero(ring->rxDescArray, ring->RxDescAllocSize);

        switch (tp->InitRxDescType)
        {
        case RX_DESC_RING_TYPE_3:
            ((RtlRxDescV3 *)&ring->rxDescArray[kRxLastDesc])->RxDescNormalDDWord4.opts1 = OSSwapHostToLittleInt32(RingEnd);
            break;
        case RX_DESC_RING_TYPE_4:
            ((RtlRxDescV4 *)&ring->rxDescArray[kRxLastDesc])->RxDescNormalDDWord2.opts1 = OSSwapHostToLittleInt32(RingEnd);
            break;
        default:
            ring->rxDescArray[kRxLastDesc].opts1 = OSSwapHostToLittleInt32(RingEnd);
            break;
        }

        for (i = 0; i < ring->num_rx_desc; i++)
        {
            ring->rxMbufArray[i] = NULL;
        }
        ring->rxNextDescIndex = 0;

        ring->rxMbufCursor = IOMbufNaturalMemoryCursor::withSpecification(PAGE_SIZE, 1);

        if (!ring->rxMbufCursor)
        {
            IOLog("ZRTL8126: Couldn't create rxMbufCursor.\n");
            goto error_segm;
        }

        /* Alloc receive buffers. */
        for (i = 0; i < ring->num_rx_desc; i++)
        {
            m = allocatePacket(rxBufferSize);

            if (!m)
            {
                IOLog("ZRTL8126: Couldn't alloc receive buffer.\n");
                goto error_buf;
            }
            ring->rxMbufArray[i] = m;

            if (ring->rxMbufCursor->getPhysicalSegments(m, &rxSegment, 1) != 1)
            {
                IOLog("ZRTL8126: getPhysicalSegments() for receive buffer failed.\n");
                goto error_buf;
            }
            opts1 = (UInt32)rxSegment.length;
            opts1 |= (i == kRxLastDesc) ? (RingEnd | DescOwn) : DescOwn;
            ring->rxDescArray[i].opts1 = OSSwapHostToLittleInt32(opts1);
            ring->rxDescArray[i].opts2 = 0;
            ring->rxDescArray[i].addr = OSSwapHostToLittleInt64(rxSegment.location);
        }
    }

    /*
     * Allocate some spare mbufs and keep them in a buffer pool, to
     * have them at hand in case replaceOrCopyPacket() fails
     * under heavy load.
     */
    sparePktHead = sparePktTail = NULL;

    for (i = 0; i < kRxNumSpareMbufs; i++)
    {
        m = allocatePacket(rxBufferSize);

        if (m)
        {
            if (sparePktHead)
            {
                mbuf_setnext(sparePktTail, m);
                sparePktTail = m;
                spareNum++;
            }
            else
            {
                sparePktHead = sparePktTail = m;
                spareNum = 1;
            }
        }
    }
    result = true;

done:
    return result;

error_buf:
    for (i = 0; i < kNumRxDesc; i++)
    {
        if (rxMbufArray[i])
        {
            freePacket(rxMbufArray[i]);
            rxMbufArray[i] = NULL;
        }
    }
    RELEASE(rxMbufCursor);

error_segm:
    rxDescDmaCmd->clearMemoryDescriptor();

error_set_desc:
    RELEASE(rxDescDmaCmd);

error_dma:
    rxBufDesc->complete();

error_prep:
    RELEASE(rxBufDesc);

error_buff:
    IOFree(rxBufArrayMem, kRxBufArraySize);
    rxBufArrayMem = NULL;
    rxMbufArray = NULL;

    goto done;
}

void ZRTL8126::markDescriptorOpts2Zero(RtlRxDesc *desc)

{
    struct rtl8126_private *tp = &linuxData;

    switch (tp->InitRxDescType)
    {
    case RX_DESC_RING_TYPE_3:
        ((RtlRxDescV3 *)desc)->RxDescNormalDDWord4.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    case RX_DESC_RING_TYPE_4:
        ((RtlRxDescV4 *)desc)->RxDescNormalDDWord2.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    default:
        desc->opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    }
}
void ZRTL8126::markDescriptorOpts1(RtlRxDesc *desc, uint32_t)

{
    struct rtl8126_private *tp = &linuxData;

    switch (tp->InitRxDescType)
    {
    case RX_DESC_RING_TYPE_3:
        ((RtlRxDescV3 *)desc)->RxDescNormalDDWord4.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    case RX_DESC_RING_TYPE_4:
        ((RtlRxDescV4 *)desc)->RxDescNormalDDWord2.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    default:
        desc->opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    }
}
void ZRTL8126::markDescriptorOpts2(RtlRxDesc *desc, uint32_t)

{
    struct rtl8126_private *tp = &linuxData;

    switch (tp->InitRxDescType)
    {
    case RX_DESC_RING_TYPE_3:
        ((RtlRxDescV3 *)desc)->RxDescNormalDDWord4.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    case RX_DESC_RING_TYPE_4:
        ((RtlRxDescV4 *)desc)->RxDescNormalDDWord2.opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    default:
        desc->opts1 = OSSwapHostToLittleInt32(RingEnd);
        break;
    }
}

void ZRTL8126::refillSpareBuffers()
{
    mbuf_t m;

    while (spareNum < kRxNumSpareMbufs)
    {
        m = allocatePacket(rxBufferSize);

        if (!m)
            break;

        mbuf_setnext(sparePktTail, m);
        sparePktTail = m;
        OSIncrementAtomic(&spareNum);
    }
}

IOReturn ZRTL8126::refillAction(OSObject *owner, void *arg1, void *arg2, void *arg3, void *arg4)
{
    ZRTL8126 *ethCtlr = OSDynamicCast(ZRTL8126, owner);

    if (ethCtlr)
    {
        ethCtlr->refillSpareBuffers();
    }
    return kIOReturnSuccess;
}

bool ZRTL8126::setupTxResources()
{
    IODMACommand::Segment64 seg;
    UInt64 offset = 0;
    UInt32 numSegs = 1;
    UInt32 i;
    bool result = false;

    /* Alloc tx mbuf_t array. */
    txBufArrayMem = IOMallocZero(kTxBufArraySize);

    if (!txBufArrayMem)
    {
        IOLog("ZRTL8126: Couldn't alloc transmit buffer array.\n");
        goto done;
    }
    txMbufArray = (mbuf_t *)txBufArrayMem;

    /* Create transmitter descriptor array. */
    txBufDesc = IOBufferMemoryDescriptor::inTaskWithPhysicalMask(kernel_task, (kIODirectionInOut | kIOMemoryPhysicallyContiguous | kIOMemoryHostPhysicallyContiguous | kIOMapInhibitCache), kTxDescSize, 0xFFFFFFFFFFFFFF00ULL);

    if (!txBufDesc)
    {
        IOLog("ZRTL8126: Couldn't alloc txBufDesc.\n");
        goto error_buff;
    }
    if (txBufDesc->prepare() != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: txBufDesc->prepare() failed.\n");
        goto error_prep;
    }
    txDescArray = (RtlTxDesc *)txBufDesc->getBytesNoCopy();

    txDescDmaCmd = IODMACommand::withSpecification(kIODMACommandOutputHost64, 64, 0, IODMACommand::kMapped, 0, 1, mapper, NULL);

    if (!txDescDmaCmd)
    {
        IOLog("ZRTL8126: Couldn't alloc txDescDmaCmd.\n");
        goto error_dma;
    }

    if (txDescDmaCmd->setMemoryDescriptor(txBufDesc) != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: setMemoryDescriptor() failed.\n");
        goto error_set_desc;
    }

    if (txDescDmaCmd->gen64IOVMSegments(&offset, &seg, &numSegs) != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: gen64IOVMSegments() failed.\n");
        goto error_segm;
    }
    /* Now get tx ring's physical address. */
    txPhyAddr = seg.fIOVMAddr;

    /* Initialize txDescArray. */
    bzero(txDescArray, kTxDescSize);
    txDescArray[kTxLastDesc].opts1 = OSSwapHostToLittleInt32(RingEnd);

    for (i = 0; i < kNumTxDesc; i++)
    {
        txMbufArray[i] = NULL;
    }
    txNextDescIndex = txDirtyDescIndex = 0;
    txTailPtr0 = txClosePtr0 = 0;
    txNumFreeDesc = kNumTxDesc;
    txMbufCursor = IOMbufNaturalMemoryCursor::withSpecification(0x1000, kMaxSegs);

    if (!txMbufCursor)
    {
        IOLog("ZRTL8126: Couldn't create txMbufCursor.\n");
        goto error_segm;
    }
    result = true;

done:
    return result;

error_segm:
    txDescDmaCmd->clearMemoryDescriptor();

error_set_desc:
    RELEASE(txDescDmaCmd);

error_dma:
    txBufDesc->complete();

error_prep:
    RELEASE(txBufDesc);

error_buff:
    IOFree(txBufArrayMem, kTxBufArraySize);
    txBufArrayMem = NULL;
    txMbufArray = NULL;

    goto done;
}

bool ZRTL8126::setupStatResources()
{
    IODMACommand::Segment64 seg;
    UInt64 offset = 0;
    UInt32 numSegs = 1;
    bool result = false;

    /* Create statistics dump buffer. */
    statBufDesc = IOBufferMemoryDescriptor::inTaskWithPhysicalMask(kernel_task, (kIODirectionIn | kIOMemoryPhysicallyContiguous | kIOMemoryHostPhysicallyContiguous | kIOMapInhibitCache), sizeof(RtlStatData), 0xFFFFFFFFFFFFFF00ULL);

    if (!statBufDesc)
    {
        IOLog("ZRTL8126: Couldn't alloc statBufDesc.\n");
        goto done;
    }

    if (statBufDesc->prepare() != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: statBufDesc->prepare() failed.\n");
        goto error_prep;
    }
    statData = (RtlStatData *)statBufDesc->getBytesNoCopy();

    statDescDmaCmd = IODMACommand::withSpecification(kIODMACommandOutputHost64, 64, 0, IODMACommand::kMapped, 0, 1);

    if (!statDescDmaCmd)
    {
        IOLog("ZRTL8126: Couldn't alloc statDescDmaCmd.\n");
        goto error_dma;
    }

    if (statDescDmaCmd->setMemoryDescriptor(statBufDesc) != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: setMemoryDescriptor() failed.\n");
        goto error_set_desc;
    }

    if (statDescDmaCmd->gen64IOVMSegments(&offset, &seg, &numSegs) != kIOReturnSuccess)
    {
        IOLog("ZRTL8126: gen64IOVMSegments() failed.\n");
        goto error_segm;
    }
    /* And the rx ring's physical address too. */
    statPhyAddr = seg.fIOVMAddr;

    /* Initialize statData. */
    bzero(statData, sizeof(RtlStatData));

    result = true;

done:
    return result;

error_segm:
    statDescDmaCmd->clearMemoryDescriptor();

error_set_desc:
    RELEASE(statDescDmaCmd);

error_dma:
    statBufDesc->complete();

error_prep:
    RELEASE(statBufDesc);
    goto done;
}

void ZRTL8126::freeRxResources()
{
    UInt32 i;

    if (rxBufDesc)
    {
        rxBufDesc->complete();
        rxBufDesc->release();
        rxBufDesc = NULL;
        rxPhyAddr = (IOPhysicalAddress64)NULL;
    }
    RELEASE(rxMbufCursor);

    for (i = 0; i < kNumRxDesc; i++)
    {
        if (rxMbufArray[i])
        {
            freePacket(rxMbufArray[i]);
            rxMbufArray[i] = NULL;
        }
    }
    if (rxDescDmaCmd)
    {
        rxDescDmaCmd->clearMemoryDescriptor();
        rxDescDmaCmd->release();
        rxDescDmaCmd = NULL;
    }
    if (rxBufArrayMem)
    {
        IOFree(rxBufArrayMem, kRxBufArraySize);
        rxBufArrayMem = NULL;
        rxMbufArray = NULL;
    }
    if (sparePktHead)
    {
        mbuf_freem(sparePktHead);
        sparePktHead = sparePktTail = NULL;
        spareNum = 0;
    }
}

void ZRTL8126::freeTxResources()
{
    if (txBufDesc)
    {
        txBufDesc->complete();
        txBufDesc->release();
        txBufDesc = NULL;
        txPhyAddr = (IOPhysicalAddress64)NULL;
    }
    if (txDescDmaCmd)
    {
        txDescDmaCmd->clearMemoryDescriptor();
        txDescDmaCmd->release();
        txDescDmaCmd = NULL;
    }
    if (txBufArrayMem)
    {
        IOFree(txBufArrayMem, kTxBufArraySize);
        txBufArrayMem = NULL;
        txMbufArray = NULL;
    }
    RELEASE(txMbufCursor);
}

void ZRTL8126::freeStatResources()
{
    if (statBufDesc)
    {
        statBufDesc->complete();
        statBufDesc->release();
        statBufDesc = NULL;
        statPhyAddr = (IOPhysicalAddress64)NULL;
    }
    if (statDescDmaCmd)
    {
        statDescDmaCmd->clearMemoryDescriptor();
        statDescDmaCmd->release();
        statDescDmaCmd = NULL;
    }
}

void ZRTL8126::clearRxTxRings()
{
    mbuf_t m;
    UInt32 lastIndex = kTxLastDesc;
    UInt32 opts1;
    UInt32 i;

    DebugLog("ZRTL8126: clearDescriptors() ===>\n");

    for (i = 0; i < kNumTxDesc; i++)
    {
        txDescArray[i].opts1 = OSSwapHostToLittleInt32((i != lastIndex) ? 0 : RingEnd);
        m = txMbufArray[i];

        if (m)
        {
            freePacket(m);
            txMbufArray[i] = NULL;
        }
    }
    txTailPtr0 = txClosePtr0 = 0;
    txDirtyDescIndex = txNextDescIndex = 0;
    txNumFreeDesc = kNumTxDesc;

    lastIndex = kRxLastDesc;

    for (i = 0; i < kNumRxDesc; i++)
    {
        opts1 = rxBufferSize;
        opts1 |= (i == kRxLastDesc) ? (RingEnd | DescOwn) : DescOwn;
        rxDescArray[i].opts1 = OSSwapHostToLittleInt32(opts1);
        rxDescArray[i].opts2 = 0;
    }
    rxNextDescIndex = 0;
    deadlockWarn = 0;

    DebugLog("ZRTL8126: clearDescriptors() <===\n");
}
